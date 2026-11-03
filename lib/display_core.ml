(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What a frame depends on: the changes it was told of, the half tenth of a
   second when the view reads the time (a spinner turns at each tenth, a
   time rounded to the tenth at each half), and the terminal's size. A view
   that reads state the display is not told of depends on more. *)
type stamp = {
  mutable changes : int;
  mutable tick : int;
  mutable width : int;
  mutable height : int;
}

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
  writes : (string * bool * string) Queue.t;
  mutable torn : bool;
  mutable salvage : string;
  mutable writing : bool;
  mutable release : unit -> unit;
  mutable on_close : unit -> unit;
  mutable on_output : (string -> unit) option;
  mutable looks : (Look.t * Look.t) option;
  mutable looked : int;
  mutable busy : int;
  idle : (unit -> unit) list Atomic.t;
}

let pp ppf t =
  Fmt.pf ppf "<display %dx%d%s%s>" t.width t.height
    (if Option.is_some t.size then " live" else "")
    (if t.closed then " closed" else "")

let is_live t = Option.is_some t.size
let since_start { now; start; _ } = now () -. start
let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(* From any column: [width] spaces from the first column fill the row and
   leave the cursor on it, from any other they wrap onto the next row; the
   carriage return then goes to the first column. *)
let fresh_row t = String.make t.width ' ' ^ "\r\027[K"

(* What a view, a hook or the output raises waits for a call that asks, so that
   drawing fails no call that did not ask for it. What only the program can
   answer does not wait: an interrupt, an exhausted heap or stack, the fiber's
   cancellation. Those have not failed the view, and go on from the call under
   way, once the terminal is restored if it is the close. *)
let is_async = Task_core.is_async

let rec is_cancelled t = function
  | Fun.Finally_raised e -> is_cancelled t e
  | e -> t.cancelled e

let urgent t e = is_async e || is_cancelled t e

(* An error is worse than a cancellation, as in [Eio.Exn.combine]: it is the
   reason, and a cancelled fiber meets its cancellation again anyway. *)
let severity t e = if is_async e then 2 else if is_cancelled t e then 0 else 1

(* The first one waits for a call that asks. *)
let keep t e =
  let bt = Printexc.get_raw_backtrace () in
  if Option.is_none t.failure then t.failure <- Some (e, bt)

(* A view that raises is dropped: what was to be written is written with the
   fallback view. *)
let fail t e =
  keep t e;
  t.view <- t.fallback

let raise_failure t =
  match t.failure with
  | None -> ()
  | Some (e, bt) ->
      t.failure <- None;
      Printexc.raise_with_backtrace e bt

(* [finally ()] once [e] was raised, with what it raises going on instead if it
   is the worse one, as itself. *)
let settle_after t e finally =
  try finally () with later when severity t later <= severity t e -> ()

(* [f ()], then [finally ()] whatever [f] raised, unlike [Fun.protect] with no
   new exception if both raise: a cancellation or an interrupt met in [finally]
   stays what it is. *)
let protect t ~finally f =
  match f () with
  | v ->
      finally ();
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      settle_after t e finally;
      Printexc.raise_with_backtrace e bt

(* Everything the display writes goes out in one piece per operation, on a
   terminal as one synchronized update (mode 2026), so that a terminal never
   shows a region half erased or half drawn. A write error (a closed pipe)
   is the end of the output, not of the program. Anything else an output raises
   is the program's, and goes on: the logs give their message to another
   reporter on it. *)

let sync_on = "\027[?2026h"
let sync_off = "\027[?2026l"
let synchronized t s = if is_live t then sync_on ^ s ^ sync_off else s

(* Without a terminal, standard output may share the display's pipe: what
   the program printed there goes out first. An error there is the
   program's, which its own writes meet once console stops flushing. *)
let stdout_failed = Atomic.make false
let write_error t = function Sys_error _ -> true | e -> t.write_error e

let writing t f =
  match f () with () -> () | exception e when write_error t e -> ()

let flush_ppf t = writing t (fun () -> Format.pp_print_flush t.ppf ())

let flush_stdout t =
  if (not (is_live t)) && t.ppf != Fmt.stdout && not (Atomic.get stdout_failed)
  then
    try Format.pp_print_flush Fmt.stdout ()
    with Sys_error _ -> Atomic.set stdout_failed true

(* A [framed] string is synchronized already. *)
let write_now t (s, framed, _) =
  writing t @@ fun () ->
  let s =
    if framed then s
    else if is_live t then synchronized t s
    else if t.partial then "\n" ^ s
    else s
  in
  let out = Format.pp_get_formatter_out_functions t.ppf () in
  out.out_string s 0 (String.length s);
  out.out_flush ()

(* A signal handler runs at any safe point: what it asks of the display
   waits until the drawing or the write under way is done. *)
let rec run_all t = function
  | [] -> ()
  | f :: rest -> protect t f ~finally:(fun () -> run_all t rest)

let rec drain t =
  match Atomic.exchange t.idle [] with
  | [] -> ()
  | fs ->
      protect t ~finally:(fun () -> drain t) (fun () -> run_all t (List.rev fs))

let busy t f =
  t.busy <- t.busy + 1;
  protect t f ~finally:(fun () ->
      t.busy <- t.busy - 1;
      if t.busy = 0 then drain t)

let rec when_idle t f =
  if t.busy = 0 then f ()
  else
    let fs = Atomic.get t.idle in
    if not (Atomic.compare_and_set t.idle fs (f :: fs)) then when_idle t f

(* A cancellation or an interrupt can cut a write anywhere, and what the output
   took of it cannot be known: going out again, a frame's moves would erase what
   is above the region. So nothing that moves goes out again. What was to be
   printed once is kept, and the region starts again on a row of its own. *)
let tear t =
  Queue.iter (fun (_, _, salvage) -> t.salvage <- t.salvage ^ salvage) t.writes;
  Queue.clear t.writes;
  t.torn <- true;
  t.frame <- [||];
  t.frame_widths <- [||]

(* What a tear left goes before what was written since. *)
let mend t =
  if t.torn then begin
    t.torn <- false;
    let salvage = t.salvage in
    t.salvage <- "";
    let next = Queue.create () in
    if is_live t then
      Queue.add (sync_off ^ "\r\n" ^ salvage, true, salvage) next
    else if salvage <> "" then Queue.add (salvage, false, salvage) next;
    Queue.transfer t.writes next;
    Queue.transfer next t.writes
  end

(* The output functions of [ppf] may yield to another fiber in the middle of a
   write: what is written meanwhile waits in [writes], and the write under way
   writes it out in turn, so that writes never mix. *)
let send_now ~framed ~salvage t s =
  if s <> "" then begin
    writing t (fun () ->
        flush_stdout t;
        Format.pp_print_flush t.ppf ());
    Queue.add (s, framed, salvage) t.writes
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
          | exception (Effect.Unhandled _ as e) ->
              (* A signal handler cannot write on an Eio flow, and wrote nothing:
                 {!flush} does it from a fiber. *)
              raise e
          | exception e when not (urgent t e) ->
              let bt = Printexc.get_raw_backtrace () in
              ignore (Queue.pop t.writes);
              Printexc.raise_with_backtrace e bt
          | exception e ->
              let bt = Printexc.get_raw_backtrace () in
              tear t;
              Printexc.raise_with_backtrace e bt
        done)
  end

let send ?(framed = false) ?(salvage = "") t s =
  busy t (fun () -> send_now ~framed ~salvage t s)

let write ?salvage t s = send ?salvage t s
let unwritten t = t.torn || not (Queue.is_empty t.writes)

(* On a terminal the carriage return is written, not left to the tty: with
   output processing off a line feed keeps the column. *)
let eol t = if is_live t then "\r\n" else "\n"

(* A frame that moves moves only in colour: without it, rain would still
   change its glyphs. *)
(* The look of the formatter, still and live, kept while its theme and its
   UTF-8 stay. *)
let look ?(live = false) t =
  let theme = Theme.get t.ppf and utf_8 = Fmt.utf_8 t.ppf in
  let still, moving =
    match t.looks with
    | Some ((l, _) as looks) when l.Look.theme == theme && l.utf_8 = utf_8 ->
        looks
    | old ->
        let still = { Look.theme; utf_8; live = false } in
        let looks = (still, { still with live = Option.is_some t.depth }) in
        (* A new theme is a change the frame shows. *)
        if Option.is_some old then t.changes <- t.changes + 1;
        t.looks <- Some looks;
        looks
  in
  if live then moving else still

let render_in look t line = View_core.render ~look ?depth:t.depth line
let render t line = render_in (look t) t line

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

(* The lines of [view] laid out in [c], the last [height] of them if given,
   each drawn in [look] and cut to the terminal's width if there is one. *)
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

(* [draw ()] from the view, and from the fallback once the view raised. *)
let or_fallback t draw =
  match draw () with
  | lines -> lines
  | exception e when not (urgent t e) -> (
      fail t e;
      match draw () with
      | lines -> lines
      | exception e when not (urgent t e) ->
          keep t e;
          [])

let spans t = or_fallback t (fun () -> frame t)
let max_size = 4096
let bounded n = min max_size (max 1 n)

(* What is printed once is still and as tall as it is: as wide as the
   terminal, or whole without one. *)
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
          t.width <- w;
          t.height <- h;
          changed)

(* No line end after the last line: on the bottom row of the screen it would
   scroll the region up on every redraw. A line is erased before it is
   written, at the first column, where erasing to the end of the line cannot
   take the last cell of a full line. *)
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
    t.frame_widths <- Array.of_list (List.map snd spans)
  end

(* A redraw with all that the frame depends on as it was draws nothing,
   computes nothing and allocates nothing. The look is read again from the
   formatter once a slot was set, such as its theme. *)
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

(* What the view reads first: reading the look again may count a change. *)
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

(* The frame's bytes, synchronized, in the display's buffer, which every
   frame reuses; a frame of no bytes writes none. *)
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

(* The region starts once the delay passed, on a row of its own, the
   cursor hidden. *)
let start_region t =
  if (not t.shown) && since_start t >= t.delay then begin
    t.shown <- true;
    write ~salvage:hide_cursor t (hide_cursor ^ fresh_row t)
  end

(* What a tear left goes out first, and no frame is added behind a write that
   does not go out: those it would replace pile up otherwise. *)
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
      (* A narrower terminal has reflowed the old frame: it is cleared with
         the old frame's rows counted at the new width. The view may have
         closed the display. *)
      if not t.closed then
        framing t (fun buf ->
            if resized then clear buf t;
            draw buf t ~resized spans)
    end
  end

let paint t = busy t (fun () -> paint_now t)

(* A producer that reports often repaints at the eye's pace. *)
let paint_if_stale t = if t.now () -. t.painted_at >= 0.1 then paint t

let touch t =
  t.changes <- t.changes + 1;
  if is_live t then paint_if_stale t

(* Lines printed once, above the region, which is drawn again below them in
   the same write, or in turn without one. *)
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
    if t.suspended = 0 && (not t.closed) && t.shown then (
      match spans t with
      | spans -> commit (if t.closed then None else Some spans)
      | exception e ->
          (* The lines are often what explains it: they still go out. *)
          let bt = Printexc.get_raw_backtrace () in
          settle_after t e (fun () -> commit None);
          Printexc.raise_with_backtrace e bt)
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

(* [tapped out note] is [out] calling [note] with the last byte of each
   write, a space for blanks. *)
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

(* [tap ppf note] makes [ppf] call [note] as {!tapped} does, and is the out
   functions [ppf] had and those it has. *)
let tap ppf note =
  let out = Format.pp_get_formatter_out_functions ppf () in
  let ours = tapped out note in
  Format.pp_set_formatter_out_functions ppf ours;
  (out, ours)

let note_line t c = t.partial <- c <> '\n'

(* Without a terminal, what the program and the display write on [ppf] is
   watched for its last byte, so that a line the display prints starts at
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

let v ?size ?once_width ?(write_error = fun _ -> false)
    ?(cancelled = fun _ -> false) ?(delay = 0.) ?(fallback = View_core.empty)
    ~now ppf view =
  let depth =
    match Fmt.style_renderer ppf with
    | `Ansi_tty -> Some (Color.depth ppf)
    | `None -> None
  in
  let t =
    {
      ppf;
      size;
      once_width;
      depth;
      now;
      start = now ();
      delay;
      shown = false;
      write_error;
      cancelled;
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
      writing = false;
      torn = false;
      salvage = "";
      release = ignore;
      on_close = ignore;
      on_output = None;
      looks = None;
      looked = -1;
      busy = 0;
      idle = Atomic.make [];
    }
  in
  ignore (refresh_size t);
  if is_live t then paint t else watch_lines t;
  t

let redraw t =
  paint t;
  raise_failure t

let set_view t view =
  if not t.closed then begin
    t.view <- view;
    t.changes <- t.changes + 1;
    paint t;
    raise_failure t
  end

(* A view printed once is laid out still, as wide as the terminal or whole
   without one, at once. *)
let print t view =
  if (not t.closed) || t.closing then begin
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
  writing t @@ fun () ->
  Format.pp_print_flush t.ppf ();
  if !last <> '\n' then begin
    let eol = synchronized t (eol t) in
    out.out_string eol 0 (String.length eol);
    out.out_flush ()
  end

(* What [f] writes is taken by [on_output]: the lines of events. *)
let taken t on_output f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_utf_8 ppf (Fmt.utf_8 t.ppf);
  Format.pp_set_margin ppf t.width;
  protect t
    (fun () -> f ppf)
    ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      on_output (Buffer.contents buf))

(* What [f] writes is watched for its last byte: output that does not end
   its line is ended, so the region starts on a line of its own. *)
let with_output t f =
  let call () =
    flush_ppf t;
    let margin = Format.pp_get_margin t.ppf () in
    let last = ref '\n' in
    let out, _ = tap t.ppf (( := ) last) in
    Format.pp_set_margin t.ppf t.width;
    protect t
      (fun () -> f t.ppf)
      ~finally:(fun () ->
        protect t
          (fun () -> end_line t out last)
          ~finally:(fun () ->
            Format.pp_set_formatter_out_functions t.ppf out;
            Format.pp_set_margin t.ppf margin))
  in
  if t.closed then f t.ppf
  else
    match t.on_output with
    | Some on_output -> taken t on_output f
    | None ->
        if (not (is_live t)) || t.suspended > 0 then call ()
        else begin
          ignore (refresh_size t);
          erase t;
          t.suspended <- t.suspended + 1;
          protect t call ~finally:(fun () ->
              t.suspended <- t.suspended - 1;
              paint t)
        end

(* What is left of closing once the last frame is known. *)
let restore t out =
  if is_live t then begin
    let buf = Buffer.create 256 in
    clear buf t;
    Buffer.add_string buf show_cursor;
    Buffer.add_string buf out;
    write ~salvage:(show_cursor ^ out) t (Buffer.contents buf)
  end
  else protect t (fun () -> write ~salvage:out t out) ~finally:t.release

(* Closed first, so that what the hook ends draws no region. Each step runs
   whatever the one before it raised, so that the terminal is restored before
   the worst of it goes on. *)
let shut t =
  t.closed <- true;
  t.closing <- true;
  protect t t.on_close ~finally:(fun () ->
      t.closing <- false;
      protect t
        (fun () -> ignore (refresh_size t))
        ~finally:(fun () ->
          let out = ref "" in
          protect t
            (fun () ->
              out :=
                String.concat ""
                  (List.map (fun l -> render t l ^ eol t) (final t)))
            ~finally:(fun () -> restore t !out)))

let closing ~before t =
  protect t before ~finally:(fun () -> if not t.closed then shut t)

let close ?(before = ignore) t =
  busy t @@ fun () ->
  protect t (fun () -> closing ~before t) ~finally:(fun () -> raise_failure t)

let close_quietly ~before t =
  busy t @@ fun () ->
  (try closing ~before t with e when not (is_async e) -> ());
  t.failure <- None

let flush t = write t ""

let reveal t =
  if is_live t && t.shown && not t.closed then
    write ~salvage:show_cursor t show_cursor

let set_cursor t visible =
  let s = if visible then show_cursor else hide_cursor in
  write ~salvage:s t s

let print_lines t lines =
  if ((not t.closed) || t.closing) && lines <> [] then commit t lines

let reporting t f =
  match f () with () -> () | exception e when is_cancelled t e -> ()

let told t = t.told <- true
let closed t = t.closed
let width t = t.width
let on_close t f = t.on_close <- f
let on_output t f = t.on_output <- Some f
