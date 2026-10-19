(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(* Everything the display writes goes out in one piece per operation, so a
   terminal never shows a region half erased or half drawn. A write error
   (a closed pipe) is the end of the output, not of the program. *)
let write t s =
  if s <> "" then
    try
      Format.pp_print_flush t.ppf ();
      let out = Format.pp_get_formatter_out_functions t.ppf () in
      out.out_string s 0 (String.length s);
      out.out_flush ()
    with Sys_error _ -> ()

let render t span = Span.render ?depth:t.depth span

(* {1 The frame} *)

(* One row stays free for the cursor: a region as tall as the screen would
   scroll its first line out of reach of the cursor moves that erase it. *)
let capacity t = max 0 (t.height - 1)

let marker hidden =
  Span.styled Style.faint (Printf.sprintf "  \xe2\x80\xa6 %d more lines" hidden)

let rec take n = function x :: l when n > 0 -> x :: take (n - 1) l | _ -> []
let rec drop n = function _ :: l when n > 0 -> drop (n - 1) l | l -> l

(* The region: the header, then each running row and its log tail, within
   {!capacity}. When they do not fit, log lines go first, the oldest first;
   then the rows between the first and the most recent; a marker counts what
   is hidden. Only the rows shown are drawn. *)
let frame t =
  let cap = capacity t in
  if cap = 0 then []
  else
    let header = Option.to_list (Display_row.header t) in
    let budget = cap - List.length header in
    let rows = lasts (running_rows t) in
    let live (r, lasts) = Display_row.live t ~lasts r in
    let n = List.length rows in
    let total =
      List.fold_left (fun acc (r, _) -> acc + 1 + Queue.length r.logs) 0 rows
    in
    if budget <= 0 then header
    else if total <= budget then
      header
      @ List.concat_map
          (fun ((r, lasts) as row) -> live row :: Display_row.tail t ~lasts r)
          rows
    else if n <= budget then begin
      let to_drop = ref (total - (budget - 1)) in
      let hidden = !to_drop in
      let body =
        List.concat_map
          (fun ((r, lasts) as row) ->
            let tail = Display_row.tail t ~lasts r in
            let k = min !to_drop (List.length tail) in
            to_drop := !to_drop - k;
            live row :: drop k tail)
          rows
      in
      header @ (marker hidden :: body)
    end
    else
      match rows with
      | [] -> header
      | first :: _ ->
          let last = List.nth rows (n - 1) in
          if budget = 1 then header @ [ live last ]
          else if budget = 2 then header @ [ live first; live last ]
          else
            let shown = drop (n - (budget - 2)) rows in
            header
            @ live first
              :: marker (total - 1 - List.length shown)
              :: List.map live shown

(* {1 Drawing} *)

let add_up buf n =
  if n > 0 then Buffer.add_string buf (Printf.sprintf "\027[%dA" n)

(* The rows the frame on screen takes at [width]: a terminal that reflows
   its lines when it narrows has wrapped each old line onto more rows. *)
let rows_on_screen t =
  Array.fold_left
    (fun acc w -> acc + max 1 ((w + t.width - 1) / max 1 t.width))
    0 t.frame_widths

(* From the end of the region's last line, to the start of its first, and
   everything below erased. *)
let clear buf t =
  let rows = rows_on_screen t in
  if rows > 0 then begin
    Buffer.add_char buf '\r';
    add_up buf (rows - 1);
    Buffer.add_string buf "\027[J"
  end;
  t.frame <- [||];
  t.frame_widths <- [||]

let refresh_size t =
  match t.size with
  | None -> false
  | Some size ->
      let w, h = size () in
      let w = max 1 w and h = max 1 h in
      let changed = w <> t.width || h <> t.height in
      t.width <- w;
      t.height <- h;
      changed

(* No line end after the last line: on the bottom row of the screen it would
   scroll the region up on every redraw. A line is erased before it is
   written, at the first column, where erasing to the end of the line cannot
   take the last cell of a full line. *)
let draw buf t ~resized =
  let spans = frame t in
  let lines = Array.of_list (List.map (render t) spans) in
  if lines <> t.frame || resized then begin
    let n = Array.length lines in
    if (not resized) && n > 0 && n = Array.length t.frame then begin
      Buffer.add_char buf '\r';
      add_up buf (n - 1);
      Array.iteri
        (fun i line ->
          if not (String.equal line t.frame.(i)) then begin
            Buffer.add_string buf "\027[K";
            Buffer.add_string buf line
          end;
          if i < n - 1 then Buffer.add_char buf '\n')
        lines
    end
    else begin
      clear buf t;
      Array.iteri
        (fun i line ->
          if i > 0 then Buffer.add_char buf '\n';
          Buffer.add_string buf line)
        lines
    end;
    t.frame <- lines;
    t.frame_widths <- Array.of_list (List.map Span.width spans)
  end

(* What a frame depends on: what the tasks were told, the tenth of a second
   the timers and spinners show, and the terminal's size. A redraw with all
   of them as they were draws nothing and computes nothing. *)
let stamp t =
  (t.changes, int_of_float ((t.now () -. t.start) *. 10.), t.width, t.height)

let paint t =
  if is_live t && (not t.closed) && t.suspended = 0 then begin
    let resized = refresh_size t in
    let stamp = stamp t in
    if resized || stamp <> t.painted || t.frame = [||] then begin
      t.painted <- stamp;
      t.painted_at <- t.now ();
      (* A narrower terminal has reflowed the old frame: it is cleared with
       the old frame's rows counted at the new width. *)
      let buf = Buffer.create 256 in
      if resized then clear buf t;
      draw buf t ~resized;
      write t (Buffer.contents buf)
    end
  end

(* A producer that reports often repaints at the eye's pace. *)
let paint_if_stale t = if t.now () -. t.painted_at >= 0.1 then paint t

(* Lines printed once, above the region, which is drawn again below them. *)
let commit t lines =
  t.changes <- t.changes + 1;
  if is_live t then begin
    ignore (refresh_size t);
    let buf = Buffer.create 256 in
    clear buf t;
    List.iter
      (fun line ->
        Buffer.add_string buf (render t line);
        Buffer.add_char buf '\n')
      lines;
    if t.suspended = 0 && not t.closed then begin
      t.painted_at <- t.now ();
      draw buf t ~resized:false
    end;
    write t (Buffer.contents buf)
  end
  else
    write t
      (String.concat "" (List.map (fun line -> render t line ^ "\n") lines))

let erase t =
  let buf = Buffer.create 32 in
  clear buf t;
  write t (Buffer.contents buf)
