(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(* From any column: [width] spaces from the first column fill the row and
   leave the cursor on it, from any other they wrap onto the next row; the
   carriage return then goes to the first column. *)
let fresh_row t = String.make t.width ' ' ^ "\r\027[K"

(* Everything the display writes goes out in one piece per operation, on a
   terminal as one synchronized update (mode 2026), so that a terminal never
   shows a region half erased or half drawn. A write error (a closed pipe)
   is the end of the output, not of the program. *)
let synchronized t s =
  if is_live t then "\027[?2026h" ^ s ^ "\027[?2026l" else s

(* Without a terminal, standard output may share the display's pipe: what
   the program printed there goes out first. An error there is the
   program's, which its own writes meet once console stops flushing. *)
let stdout_failed = Atomic.make false

let flush_stdout t =
  if (not (is_live t)) && t.ppf != Fmt.stdout && not (Atomic.get stdout_failed)
  then
    try Format.pp_print_flush Fmt.stdout ()
    with Sys_error _ -> Atomic.set stdout_failed true

let write_now t s =
  try
    let s =
      if is_live t then synchronized t s else if t.partial then "\n" ^ s else s
    in
    let out = Format.pp_get_formatter_out_functions t.ppf () in
    out.out_string s 0 (String.length s);
    out.out_flush ()
  with e when write_error t e -> ()

(* The output functions of [ppf] may yield to another fiber (an Eio flow)
   in the middle of a write: what is written meanwhile waits in [writes],
   and the write under way writes it out in turn, so that writes never mix.
   A write cut short (a cancelled fiber) leaves the rest to the next. *)
let write t s =
  if s <> "" then begin
    (try
       flush_stdout t;
       Format.pp_print_flush t.ppf ()
     with e when write_error t e -> ());
    Queue.add s t.writes
  end;
  if not t.writing then begin
    t.writing <- true;
    Fun.protect
      ~finally:(fun () -> t.writing <- false)
      (fun () ->
        while not (Queue.is_empty t.writes) do
          write_now t (Queue.pop t.writes)
        done)
  end

(* A frame that moves moves only in colour: without it, rain would still
   change its glyphs. *)
let moving t =
  let look = Widget.look t.ppf in
  if Option.is_some t.depth then
    { look with elapsed = Some (t.now () -. t.start) }
  else look

let render t span = Span_core.render ~look:(moving t) ?depth:t.depth span

(* On a terminal the carriage return is written, not left to the tty: with
   output processing off a line feed keeps the column. *)
let eol t = if is_live t then "\r\n" else "\n"

(* {1 The frame} *)

(* One row stays free for the cursor: a region as tall as the screen would
   scroll its first line out of reach of the cursor moves that erase it. *)
let capacity t = max 0 (t.height - 1)

(* The last [height] lines of [lines], those of a block cut in its height
   dropped with the rest of it, so that no fragment of a table's or a
   panel's frame shows. *)
let last height lines =
  let extra = List.length lines - height in
  if extra <= 0 then List.map snd lines
  else
    let rec past_block = function
      | (true, _) :: rest -> past_block rest
      | rest -> rest
    in
    List.map snd (past_block (List.drop extra lines))

(* The last lines of the view in [look] that fit in [height], each cut to
   the terminal's width if there is one. The default view's lines are
   taken as they are drawn, without joining them into a span to split
   again. *)
let lines t ~look ~height view =
  let lines =
    match view with
    | None ->
        List.map
          (fun l -> (false, Span_core.resolve look l))
          (Display_tree.default_lines t ~width:t.width ~height)
    | Some view -> (
        match view t ~width:t.width ~height with
        | span when Span_core.is_empty span -> []
        | span -> Span_core.seamed (Span_core.resolve look span))
  in
  let lines = last height lines in
  if is_live t then List.map (Widget.truncate look t.width) lines else lines

let frame t =
  match capacity t with
  | 0 -> []
  | height -> lines t ~look:(moving t) ~height t.view

(* A view that raises is dropped: what was to be written is written with
   the default view, then the exception is raised again. *)
let framed t k =
  match frame t with
  | spans -> k spans
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      t.view <- None;
      k (frame t);
      Printexc.raise_with_backtrace e bt

(* The last frame is still, as tall as it is, and drawn by the default
   view if the program's raises. *)
let final t =
  t.closing <- true;
  let look = Widget.look t.ppf in
  Fun.protect
    ~finally:(fun () -> t.closing <- false)
    (fun () ->
      match lines t ~look ~height:max_int t.view with
      | lines -> lines
      | exception _ ->
          t.view <- None;
          lines t ~look ~height:max_int None)

(* {1 Drawing} *)

let add_up buf n =
  if n > 0 then begin
    Buffer.add_string buf "\027[";
    Buffer.add_string buf (string_of_int n);
    Buffer.add_char buf 'A'
  end

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

(* No screen is this wide or tall, and the display writes a row of its
   width when it starts: a larger size is taken as this one. *)
let max_size = 4096
let bounded n = min max_size (max 1 n)

let refresh_size t =
  match t.size with
  | None -> false
  | Some size ->
      let w, h = size () in
      let w = bounded w and h = bounded h in
      let changed = w <> t.width || h <> t.height in
      t.width <- w;
      t.height <- h;
      changed

(* No line end after the last line: on the bottom row of the screen it would
   scroll the region up on every redraw. A line is erased before it is
   written, at the first column, where erasing to the end of the line cannot
   take the last cell of a full line. *)
let draw buf t ~resized spans =
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
          if i < n - 1 then Buffer.add_string buf "\r\n")
        lines
    end
    else begin
      (* The region drawn afresh starts where the cursor is, which output
         the display did not see may have left anywhere. *)
      clear buf t;
      Array.iteri
        (fun i line ->
          Buffer.add_string buf (if i > 0 then "\r\n\027[K" else "\r\027[K");
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
    (* A view may show what the display is not told of. *)
    if resized || stamp <> t.painted || t.frame = [||] || Option.is_some t.view
    then begin
      t.painted <- stamp;
      t.painted_at <- t.now ();
      (* A narrower terminal has reflowed the old frame: it is cleared with
       the old frame's rows counted at the new width. *)
      framed t (fun spans ->
          let buf = Buffer.create 256 in
          if resized then clear buf t;
          draw buf t ~resized spans;
          write t (Buffer.contents buf))
    end
  end

(* A producer that reports often repaints at the eye's pace. *)
let paint_if_stale t = if t.now () -. t.painted_at >= 0.1 then paint t

(* Lines printed once, above the region, which is drawn again below them. *)
let commit t lines =
  t.changes <- t.changes + 1;
  if is_live t then begin
    ignore (refresh_size t);
    let commit spans =
      let buf = Buffer.create 256 in
      clear buf t;
      List.iter
        (fun line ->
          Buffer.add_string buf (render t line);
          Buffer.add_string buf "\r\n")
        lines;
      Option.iter
        (fun spans ->
          t.painted_at <- t.now ();
          draw buf t ~resized:false spans)
        spans;
      write t (Buffer.contents buf)
    in
    if t.suspended = 0 && not t.closed then
      framed t (fun spans -> commit (Some spans))
    else commit None
  end
  else
    write t
      (String.concat "" (List.map (fun line -> render t line ^ "\n") lines))

let erase t =
  let buf = Buffer.create 32 in
  clear buf t;
  write t (Buffer.contents buf)
