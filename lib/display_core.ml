(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What the frame on screen was drawn from: the number of changes that the
   display was told of, the time in twentieths of a second when the view reads
   the time, and the size of the terminal. The time is in twentieths because a
   spinner turns every tenth, and a time rounded to the tenth changes on the
   half. A view that reads state that the display is not told of ([live]) is
   drawn again at every paint. *)
type stamp = {
  mutable changes : int;
  mutable tick : int;
  mutable width : int;
  mutable height : int;
}

(* A queued write: [text] as it goes out, already synchronized if [framed],
   and its [salvage], the part that still goes out if the write is cut
   short. *)
type pending = { text : string; framed : bool; salvage : string }

type t = {
  ppf : Format.formatter;
  size : (unit -> int * int) option;
  once_width : (unit -> int) option;
  depth : Color.depth option;
  now : unit -> float;
  start : float;
  delay : float;
  mutable shown : bool;
  write_error : exn -> bool;
  cancelled : exn -> bool;
  atomic : (unit -> unit) -> unit;
  mutable width : int;
  mutable height : int;
  mutable view : View_core.t;
  fallback : View_core.t;
  mutable failure : (exn * Printexc.raw_backtrace) option;
  mutable frame : string array;
  mutable frame_widths : int array;
  mutable painted_at : float;
  mutable changes : int;
  mutable told : bool;
  painted : stamp;
  buf : Buffer.t;
  mutable closed : bool;
  mutable closing : bool;
  mutable suspended : int;
  mutable partial : bool;
  writes : pending Queue.t;
  mutable torn : bool;
  mutable left : string;
  mutable writing : bool;
  mutable release : unit -> unit;
  mutable on_close : unit -> unit;
  mutable on_print : unit -> unit;
  mutable on_output : (string -> unit) option;
  mutable looks : (Look.t * Look.t) option;
  mutable looked : int;
  mutable busy : int;
  idle : (unit -> unit) list Atomic.t;
  owner : Domain.id;
  latest_width : int Atomic.t;
  mutable settle : unit -> unit;
}

let pp ppf t =
  Fmt.pf ppf "<display %dx%d%s%s>" t.width t.height
    (if Option.is_some t.size then " live" else "")
    (if t.closed then " closed" else "")

let is_live t = Option.is_some t.size
let since_start { now; start; _ } = now () -. start
let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(* A row of its own, from any column. Written from the first column, [width]
   spaces fill the row and leave the cursor on it. Written from any other
   column, they wrap onto the next row. The carriage return then goes to the
   first column. *)
let fresh_row t = String.make t.width ' ' ^ "\r\027[K"

(* An exception raised by a view, a hook, the size function or the output is
   kept in [failure]. The next call that reports failures raises it. So
   drawing never fails an unrelated call, and closing restores the terminal
   first. An interrupt, an exhausted heap or stack, or the cancellation of the
   fiber is not kept: the view has not failed, and only the program can handle
   these. *)
let urgent t e = Task_core.is_async e || t.cancelled e
let protect = Task_core.protect

(* Only the first failure is kept until a call raises it. *)
let keep t e =
  let bt = Printexc.get_raw_backtrace () in
  if Option.is_none t.failure then t.failure <- Some (e, bt)

(* A view that raises is replaced by the fallback view, which draws the
   frame instead. *)
let fail t e =
  keep t e;
  t.view <- t.fallback

let raise_failure t =
  match t.failure with
  | None -> ()
  | Some (e, bt) ->
      t.failure <- None;
      Printexc.raise_with_backtrace e bt

let caught t ~default f =
  match f () with
  | v -> v
  | exception e when not (urgent t e) ->
      keep t e;
      default

(* Each operation of the display writes its output in one piece. On a
   terminal, it is one synchronized update (mode 2026), so the terminal never
   shows a region half erased or half drawn. A write error, such as a closed
   pipe, ends the output of the display and nothing else. *)

let sync_on = "\027[?2026h"
let sync_off = "\027[?2026l"
let synchronized t s = if is_live t then sync_on ^ s ^ sync_off else s

(* Without a terminal, standard output may share the pipe of the display. So
   standard output is flushed first, and what the program printed there goes
   out first. A write error on standard output belongs to the program: after
   one, console stops flushing it, and the program's own writes meet the
   error. *)
let stdout_failed = Atomic.make false
let write_error t = function Sys_error _ -> true | e -> t.write_error e

let flush_ppf t =
  try Format.pp_print_flush t.ppf () with e when write_error t e -> ()

let flush_stdout t =
  if (not (is_live t)) && t.ppf != Fmt.stdout && not (Atomic.get stdout_failed)
  then
    try Format.pp_print_flush Fmt.stdout ()
    with Sys_error _ -> Atomic.set stdout_failed true

let write_now t { text; framed; _ } =
  try
    let s =
      if framed then text
      else if is_live t then synchronized t text
      else if t.partial then "\n" ^ text
      else text
    in
    let out = Format.pp_get_formatter_out_functions t.ppf () in
    t.atomic (fun () ->
        out.out_string s 0 (String.length s);
        out.out_flush ())
  with e when write_error t e -> ()

(* A signal handler can run at any safe point. So what it asks of the display
   ([when_idle]) waits in [idle] until the current drawing or write is done. *)
let rec drain t =
  match Atomic.exchange t.idle [] with
  | [] -> ()
  | fs ->
      protect
        (fun () -> Task_core.run_all (List.rev fs))
        ~finally:(fun () -> drain t)

let busy t f =
  t.busy <- t.busy + 1;
  protect f ~finally:(fun () ->
      t.busy <- t.busy - 1;
      if t.busy = 0 then drain t)

let foreign t = not (Int.equal (Domain.self () :> int) (t.owner :> int))

let rec post t f =
  let fs = Atomic.get t.idle in
  if not (Atomic.compare_and_set t.idle fs (f :: fs)) then post t f

let when_idle t f = if t.busy = 0 && not (foreign t) then f () else post t f

(* A request from another domain waits for the domain that runs the display.
   The owner did not make that request, so an exception that the request
   raises is dropped, unless it is an interrupt or an exhausted heap or
   stack. *)
let routed t f =
  post t (fun () -> try f () with e when not (Task_core.is_async e) -> ())

(* The owner first does the requests that wait, then what it is called for. *)
let settle t =
  if t.busy = 0 then begin
    t.settle ();
    drain t
  end

let rec owned t f =
  if foreign t then routed t (fun () -> owned t f)
  else begin
    settle t;
    f ()
  end

(* An interrupt or a cancellation can cut a write anywhere, and how much of
   the write the output took is unknown. If a frame were written again, its
   cursor moves would start from where the first part left the cursor, and
   could erase what is above the region. So no queued write goes out again.
   Only the text meant to be printed once, the [salvage] of each write, is
   kept, and the region starts again on a row of its own. *)
let tear t =
  Queue.iter (fun (w : pending) -> t.left <- t.left ^ w.salvage) t.writes;
  Queue.clear t.writes;
  t.torn <- true;
  t.frame <- [||];
  t.frame_widths <- [||]

(* The salvage of a tear goes out before anything queued after the tear. On a
   terminal, it first ends the cut update, moves to a row of its own and
   erases the rows below. *)
let mend t =
  if t.torn then begin
    t.torn <- false;
    let salvage = t.left in
    t.left <- "";
    let next = Queue.create () in
    if is_live t then
      Queue.add
        { text = sync_off ^ "\r\n\027[J" ^ salvage; framed = true; salvage }
        next
    else if salvage <> "" then
      Queue.add { text = salvage; framed = false; salvage } next;
    Queue.transfer t.writes next;
    Queue.transfer next t.writes
  end

(* The output functions of [ppf] may yield to another fiber (an Eio flow) in
   the middle of a write. A write from that fiber waits in [writes], and the
   write under way writes it out after its own text, so writes never
   interleave. *)
let send_now ~framed ~salvage t s =
  if s <> "" then begin
    (try
       flush_stdout t;
       Format.pp_print_flush t.ppf ()
     with e when write_error t e -> ());
    Queue.add { text = s; framed; salvage } t.writes
  end;
  if not t.writing then begin
    t.writing <- true;
    Fun.protect
      ~finally:(fun () -> t.writing <- false)
      (fun () ->
        mend t;
        while not (Queue.is_empty t.writes) do
          match write_now t (Queue.peek t.writes) with
          | () -> ignore (Queue.pop t.writes)
          | exception e when urgent t e ->
              let bt = Printexc.get_raw_backtrace () in
              tear t;
              Printexc.raise_with_backtrace e bt
          | exception e ->
              let bt = Printexc.get_raw_backtrace () in
              ignore (Queue.pop t.writes);
              Printexc.raise_with_backtrace e bt
        done)
  end

let send ?(framed = false) ?(salvage = "") t s =
  busy t (fun () -> send_now ~framed ~salvage t s)

let write ?salvage t s = send ?salvage t s
let unwritten t = t.torn || not (Queue.is_empty t.writes)

(* On a terminal the carriage return is written explicitly: with output
   processing off, a line feed keeps the column. *)
let eol t = if is_live t then "\r\n" else "\n"

(* The look of the formatter, still and live, cached while its theme and its
   UTF-8 setting do not change. The live look moves only in colour ([depth]):
   without colour, rain would still change its glyphs. *)
let look ?(live = false) t =
  let theme = Theme.get t.ppf and utf_8 = Fmt.utf_8 t.ppf in
  let still, moving =
    match t.looks with
    | Some ((l, _) as looks) when l.Look.theme == theme && l.utf_8 = utf_8 ->
        looks
    | old ->
        let still = { Look.theme; utf_8; live = false } in
        let looks = (still, { still with live = Option.is_some t.depth }) in
        (* A new theme counts as a change, so the frame is drawn again. *)
        if Option.is_some old then t.changes <- t.changes + 1;
        t.looks <- Some looks;
        looks
  in
  if live then moving else still

let render_in look t line = View_core.render ~look ?depth:t.depth line
let render t line = render_in (look t) t line

(* One row stays free for the cursor. A region as tall as the screen would
   scroll its first line out of reach of the cursor moves that erase it. *)
let capacity t = max 0 (t.height - 1)

(* The last [height] lines of [lines]. When the top edge cuts a block, the
   rest of the block is dropped too, so no part of the frame of a table or a
   panel shows. *)
let last height lines =
  let extra = List.length lines - height in
  if extra <= 0 then List.map snd lines
  else
    let rec past_block = function
      | (true, _) :: rest -> past_block rest
      | rest -> rest
    in
    List.map snd (past_block (List.drop extra lines))

(* The lines of [view] laid out in [c], each drawn in [look]. With [height],
   only the last [height] lines. With a terminal, each line is cut to the
   width of the terminal. *)
let lines t ~look ?height c view =
  let lines =
    match View_core.resolve c view with
    | v when View_core.is_empty v -> []
    | v -> (
        let seamed = View_core.seamed (View_core.paint look v) in
        match height with
        | Some h -> last h seamed
        | None -> List.map snd seamed)
  in
  if is_live t then
    List.map
      (fun l ->
        let w = View_core.width l in
        if w > t.width then
          let l = Widget.truncate look t.width l in
          (l, View_core.width l)
        else (l, w))
      lines
  else List.map (fun l -> (l, 0)) lines

let frame t =
  match capacity t with
  | 0 -> []
  | height ->
      let look = look ~live:true t in
      let c = Ctx.v ~width:t.width ~height ~time:(since_start t) look in
      lines t ~look ~height c t.view

(* [or_fallback t draw] is [draw ()]. If the view raises, [draw] is called
   again with the fallback view. If the fallback raises too, there is no
   line. *)
let or_fallback t draw =
  match draw () with
  | lines -> lines
  | exception e when not (urgent t e) -> (
      fail t e;
      try draw () with e when not (urgent t e) -> [])

let spans t = or_fallback t (fun () -> frame t)

(* No screen is this wide or tall. The display writes a row of its width when
   it starts, so a larger size is taken as this one. *)
let max_size = 4096
let bounded n = min max_size (max 1 n)

(* Text printed once is laid out at time 0 in the still look, with no height
   bound. With a terminal, it is as wide as the terminal. Without one, it is
   as wide as [once_width] gives, or unbounded. *)
let once t =
  let look = look t in
  let width =
    if is_live t then Some t.width
    else Option.map (fun w -> bounded (w ())) t.once_width
  in
  (look, Ctx.v ?width look)

(* A view that raises in the last frame gives way to the fallback. *)
let final t =
  let look, c = once t in
  List.map fst (or_fallback t (fun () -> lines t ~look c t.view))

let add_up buf n =
  if n > 0 then begin
    Buffer.add_string buf "\027[";
    Buffer.add_string buf (string_of_int n);
    Buffer.add_char buf 'A'
  end

(* The rows that the frame on screen takes at [width]. When a terminal that
   reflows its lines narrows, it wraps each old line onto more rows. *)
let rows_on_screen t =
  Array.fold_left
    (fun acc w -> acc + max 1 ((w + t.width - 1) / max 1 t.width))
    0 t.frame_widths

(* Moves the cursor from the end of the last line of the region to the start
   of its first line, and erases everything below. *)
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
  | Some size -> (
      match size () with
      | exception e when not (urgent t e) ->
          keep t e;
          false
      | w, h ->
          let w = bounded w and h = bounded h in
          let changed = w <> t.width || h <> t.height in
          if w <> t.width then Atomic.set t.latest_width w;
          t.width <- w;
          t.height <- h;
          changed)

(* No line end follows the last line: on the bottom row of the screen, it
   would scroll the region up at every redraw. Each line is erased from the
   first column before it is written, so the erase cannot take the last cell
   of a full line. *)
let draw buf t ~resized spans =
  let look = look t in
  let lines =
    Array.of_list (List.map (fun (l, _) -> render_in look t l) spans)
  in
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
      (* A region drawn from scratch starts at the cursor. Output that the
         display did not see may have moved the cursor anywhere. *)
      clear buf t;
      Array.iteri
        (fun i line ->
          Buffer.add_string buf (if i > 0 then "\r\n\027[K" else "\r\027[K");
          Buffer.add_string buf line)
        lines
    end;
    t.frame <- lines;
    t.frame_widths <- Array.of_list (List.map snd spans)
  end

(* When nothing that the frame depends on has changed, a redraw draws,
   computes and allocates nothing. The look is read again from the formatter
   only after a slot, such as its theme, was set. *)
let reads t =
  let look =
    match t.looks with
    | Some (_, moving) when t.looked = Slot.generation () -> moving
    | _ ->
        t.looked <- Slot.generation ();
        look ~live:true t
  in
  View_core.kind t.view look

let tick t (k : View_core.kind) =
  if k.timed then int_of_float (since_start t *. 20.) else 0

(* [reads t] comes first, because reading the look again may count a
   change. *)
let stale t =
  let k = reads t in
  let tick = tick t k and p = t.painted in
  k.live || (k.tasks && not t.told) || p.changes <> t.changes || p.tick <> tick
  || p.width <> t.width || p.height <> t.height

let mark t =
  let p = t.painted in
  p.changes <- t.changes;
  p.tick <- tick t (reads t);
  p.width <- t.width;
  p.height <- t.height;
  t.painted_at <- t.now ()

(* [framing t f] writes what [f] adds to the buffer of the display as one
   synchronized update. Every frame reuses this buffer. When [f] adds nothing,
   only the writes already queued go out. *)
let framing ?salvage t f =
  let buf = t.buf in
  Buffer.clear buf;
  Buffer.add_string buf sync_on;
  f buf;
  if Buffer.length buf = String.length sync_on then write t ""
  else begin
    Buffer.add_string buf sync_off;
    let s = Buffer.contents buf in
    if Buffer.length buf > 65536 then Buffer.reset buf;
    send ~framed:true ?salvage t s
  end

(* Once the delay has passed, the region starts on a row of its own, with the
   cursor hidden. *)
let start_region t =
  if (not t.shown) && since_start t >= t.delay then begin
    t.shown <- true;
    write ~salvage:hide_cursor t (hide_cursor ^ fresh_row t)
  end

(* The salvage of a tear goes out first. No frame is queued behind a write
   that did not go out. Otherwise frames that the next frame replaces would
   pile up. *)
let paintable t =
  if (not t.closed) && unwritten t then write t "";
  is_live t && (not t.closed) && t.suspended = 0 && not (unwritten t)

let paint_now t =
  if paintable t then begin
    let resized = refresh_size t in
    start_region t;
    if t.shown && (resized || stale t || t.frame = [||]) then begin
      mark t;
      let spans = spans t in
      (* A narrower terminal has reflowed the old frame, so the frame is
         cleared with its rows counted at the new width. Drawing the view may
         have closed the display. *)
      if not t.closed then
        framing t (fun buf ->
            if resized then clear buf t;
            draw buf t ~resized spans)
    end
  end

let paint t = busy t (fun () -> paint_now t)

(* A producer that reports often repaints at most every tenth of a
   second. *)
let paint_if_stale t = if t.now () -. t.painted_at >= 0.1 then paint t

let touch t =
  t.changes <- t.changes + 1;
  if is_live t then paint_if_stale t

(* [commit_now t lines] prints [lines] once, above the region. On a terminal
   the region is drawn again below them in the same write. *)
let commit_now t lines =
  t.changes <- t.changes + 1;
  let look = look t in
  if is_live t then begin
    ignore (refresh_size t);
    let text =
      String.concat ""
        (List.map (fun line -> render_in look t line ^ "\r\n") lines)
    in
    let commit spans =
      framing ~salvage:text t (fun buf ->
          clear buf t;
          Buffer.add_string buf text;
          Option.iter
            (fun spans ->
              mark t;
              draw buf t ~resized:false spans)
            spans)
    in
    if t.suspended = 0 && not t.closed then start_region t;
    if t.suspended = 0 && (not t.closed) && t.shown then begin
      (* The lines often explain an interrupt raised while the frame was
         drawn, so they still go out. *)
      let drawn = ref None in
      protect
        (fun () -> drawn := Some (spans t))
        ~finally:(fun () -> commit (if t.closed then None else !drawn))
    end
    else commit None
  end
  else
    let text =
      String.concat ""
        (List.map (fun line -> render_in look t line ^ "\n") lines)
    in
    write ~salvage:text t text

let commit t lines = busy t (fun () -> commit_now t lines)

let erase t =
  busy t @@ fun () ->
  let buf = Buffer.create 32 in
  clear buf t;
  write t (Buffer.contents buf)

(* [tapped out note] is [out], but it also calls [note] with the last byte of
   each write, and with a space for spaces and indentation. *)
let tapped (out : Format.formatter_out_functions) note =
  let out_string s o l =
    if l > 0 then note s.[o + l - 1];
    out.out_string s o l
  in
  let out_newline () =
    note '\n';
    out.out_newline ()
  in
  let blank write n =
    if n > 0 then note ' ';
    write n
  in
  {
    out with
    out_string;
    out_newline;
    out_spaces = blank out.out_spaces;
    out_indent = blank out.out_indent;
  }

(* [tap ppf note] makes [ppf] call [note] as {!tapped} does. It returns the
   out functions that [ppf] had before and the ones it has now. *)
let tap ppf note =
  let out = Format.pp_get_formatter_out_functions ppf () in
  let ours = tapped out note in
  Format.pp_set_formatter_out_functions ppf ours;
  (out, ours)

let note_line t c = t.partial <- c <> '\n'

(* Without a terminal, the last byte of what the program and the display
   write on [ppf] is watched, so that a line printed by the display starts at
   the first column. *)
let watch_lines t =
  flush_ppf t;
  let out, ours = tap t.ppf (note_line t) in
  t.release <-
    (fun () ->
      let now = Format.pp_get_formatter_out_functions t.ppf () in
      if now.out_string == ours.out_string then
        Format.pp_set_formatter_out_functions t.ppf out)

let watch t out = tapped out (note_line t)

let started t =
  ignore (refresh_size t);
  if is_live t then paint t else watch_lines t;
  t

let depth_of ppf =
  match Fmt.style_renderer ppf with
  | `Ansi_tty -> Some (Color.depth ppf)
  | `None -> None

let v ?size ?once_width ?(write_error = fun _ -> false)
    ?(cancelled = fun _ -> false) ?(atomic = fun f -> f ()) ?(delay = 0.)
    ?(fallback = View_core.empty) ~now ppf view =
  started
    {
      ppf;
      size;
      once_width;
      depth = depth_of ppf;
      now;
      start = now ();
      delay;
      shown = false;
      write_error;
      cancelled;
      atomic;
      width = 80;
      height = 24;
      view;
      fallback;
      failure = None;
      frame = [||];
      frame_widths = [||];
      painted_at = neg_infinity;
      changes = 0;
      told = false;
      painted = { changes = -1; tick = -1; width = -1; height = -1 };
      buf = Buffer.create 4096;
      closed = false;
      closing = false;
      suspended = 0;
      partial = false;
      writes = Queue.create ();
      torn = false;
      left = "";
      writing = false;
      release = ignore;
      on_close = ignore;
      on_print = ignore;
      on_output = None;
      looks = None;
      looked = -1;
      busy = 0;
      idle = Atomic.make [];
      owner = Domain.self ();
      latest_width = Atomic.make 80;
      settle = ignore;
    }

let redraw t =
  if foreign t then routed t (fun () -> paint t)
  else begin
    settle t;
    paint t;
    raise_failure t
  end

let set_view t view =
  owned t @@ fun () ->
  if not t.closed then begin
    t.view <- view;
    t.changes <- t.changes + 1;
    paint t;
    raise_failure t
  end

let print t view =
  owned t @@ fun () ->
  if (not t.closed) || t.closing then begin
    t.on_print ();
    ignore (refresh_size t);
    let look, c = once t in
    match (lines t ~look c view, t.on_output) with
    | [], _ -> touch t
    | lines, Some on_output ->
        on_output
          (String.concat "" (List.map (fun (l, _) -> render t l ^ "\n") lines))
    | lines, None -> commit t (List.map fst lines)
  end

let end_line t (out : Format.formatter_out_functions) last =
  try
    Format.pp_print_flush t.ppf ();
    if !last <> '\n' then begin
      let eol = synchronized t (eol t) in
      out.out_string eol 0 (String.length eol);
      out.out_flush ()
    end
  with e when write_error t e -> ()

(* What [f] writes goes to [on_output] instead of [ppf]. *)
let taken t on_output f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_utf_8 ppf (Fmt.utf_8 t.ppf);
  Format.pp_set_margin ppf (Atomic.get t.latest_width);
  protect
    (fun () -> f ppf)
    ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      on_output (Buffer.contents buf))

(* The last byte of what [f] writes is watched. Output that does not end its
   line is ended, so the region starts on a line of its own. *)
let with_output_here t f =
  let call () =
    flush_ppf t;
    let margin = Format.pp_get_margin t.ppf () in
    let last = ref '\n' in
    let out, _ = tap t.ppf (( := ) last) in
    Format.pp_set_margin t.ppf t.width;
    protect
      (fun () -> f t.ppf)
      ~finally:(fun () ->
        protect
          (fun () -> end_line t out last)
          ~finally:(fun () ->
            Format.pp_set_formatter_out_functions t.ppf out;
            Format.pp_set_margin t.ppf margin))
  in
  if t.closed then f t.ppf
  else begin
    t.on_print ();
    match t.on_output with
    | Some on_output -> taken t on_output f
    | None ->
        if (not (is_live t)) || t.suspended > 0 then call ()
        else begin
          ignore (refresh_size t);
          erase t;
          t.suspended <- t.suspended + 1;
          protect call ~finally:(fun () ->
              t.suspended <- t.suspended - 1;
              paint t)
        end
  end

let with_output t f =
  if foreign t then
    taken t
      (fun s ->
        routed t (fun () ->
            settle t;
            with_output_here t (fun ppf -> Format.pp_print_string ppf s)))
      f
  else begin
    settle t;
    with_output_here t f
  end

(* The last step of closing, given the last frame [out]. On a terminal, it
   erases the region, shows the cursor and writes [out]. Otherwise it writes
   [out] and gives the formatter its own output functions back. *)
let restore t out =
  if is_live t then begin
    let buf = Buffer.create 256 in
    clear buf t;
    Buffer.add_string buf show_cursor;
    Buffer.add_string buf out;
    write ~salvage:(show_cursor ^ out) t (Buffer.contents buf)
  end
  else protect (fun () -> write ~salvage:out t out) ~finally:t.release

(* The display is marked closed first, so that what [on_close] ends draws no
   region. Each step runs even if the steps before it raised, so the terminal
   is restored before the worst exception propagates. *)
let shut t =
  t.closed <- true;
  t.closing <- true;
  let out = ref "" in
  Task_core.run_all
    [
      t.on_close;
      (fun () -> t.on_print ());
      (fun () ->
        t.closing <- false;
        ignore (refresh_size t));
      (fun () ->
        out :=
          String.concat "" (List.map (fun l -> render t l ^ eol t) (final t)));
      (fun () -> restore t !out);
    ]

let close t =
  owned t @@ fun () ->
  busy t @@ fun () ->
  protect
    (fun () -> if not t.closed then shut t)
    ~finally:(fun () -> raise_failure t)

let reveal t =
  if is_live t && t.shown && not t.closed then
    write ~salvage:show_cursor t show_cursor

let reporting t f x =
  match f x with () -> () | exception e when t.cancelled e -> ()

let writing t = t.writing

let restart t =
  busy t @@ fun () ->
  if is_live t && t.shown && (not t.closed) && not t.writing then begin
    tear t;
    write ~salvage:hide_cursor t hide_cursor
  end

let print_lines t lines =
  if ((not t.closed) || t.closing) && lines <> [] then begin
    t.on_print ();
    commit t lines
  end

let told t = t.told <- true
let closed t = t.closed
let width t = t.width
let height t = t.height
let on_close t f = t.on_close <- f
let on_print t f = t.on_print <- f
let drawing t = t.busy > 0 || t.writing
let on_settle t f = t.settle <- f
let on_output t f = t.on_output <- Some f
