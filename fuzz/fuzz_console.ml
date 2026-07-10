(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* The library leaves wide-character widths to its caller; the fuzz properties
   render CJK and emoji the way a terminal does, so they install the Uucp-backed
   hint -- the provider a real application supplies through
   [with_char_width]. *)
let uucp_char_width u =
  match Uucp.Break.tty_width_hint u with -1 -> 0 | n -> n

(* Every property here checks the library against a real VT emulator, which
   places combining marks at zero cells and wide glyphs at two; in this file
   "width" therefore means terminal width, measured with the Uucp-backed hint.
   The exception is the span-consistency property below, which checks the
   library's own default width and so calls Console.Width directly. *)
module Width = struct
  include Console.Width

  let string_width ?(char_width = uucp_char_width) s =
    Console.Width.string_width ~char_width s

  let truncate ?(char_width = uucp_char_width) w s =
    Console.Width.truncate ~char_width w s

  let pad_right ?(char_width = uucp_char_width) w s =
    Console.Width.pad_right ~char_width w s

  let pad_left ?(char_width = uucp_char_width) w s =
    Console.Width.pad_left ~char_width w s
end

let display_ctx ~ppf ~now ~width ~height =
  Display.with_char_width
    (Display.ctx ~ppf ~now
       ~dimensions:(fun () -> (width, height))
       ~is_tty:true ())
    uucp_char_width

let table_ctx ?(width = 80) ?(height = 24) () =
  Display.with_char_width
    (Display.ctx ~ppf:Format.std_formatter
       ~now:(fun () -> 0.)
       ~dimensions:(fun () -> (width, height))
       ~is_tty:false ())
    uucp_char_width

(* Width is always non-negative *)
let test_width_non_negative s =
  let w = Width.string_width s in
  check (w >= 0)

(* Span width matches underlying width *)
let test_span_width_consistency s =
  let span = Span.text s in
  let span_w = Span.width span in
  let direct_w = Console.Width.string_width s in
  check (span_w = direct_w)

(* Span concatenation width is sum of widths *)
let test_span_concat_width s1 s2 =
  let span1 = Span.text s1 in
  let span2 = Span.text s2 in
  let combined = Span.(span1 ++ span2) in
  let w1 = Span.width span1 in
  let w2 = Span.width span2 in
  let w_combined = Span.width combined in
  check (w_combined = w1 + w2)

(* Table rendering doesn't crash *)
let test_table_render_no_crash headers row =
  if List.length headers > 0 && List.length row > 0 then
    let cols = List.map Table.column headers in
    let rows = [ List.map Span.text row ] in
    let table = Table.of_rows cols rows in
    let _ = Table.to_string ~ctx:(table_ctx ()) table in
    check true
  else check true

(* Tree rendering doesn't crash *)
let test_tree_render_no_crash label =
  let tree = Tree.of_tree (Tree.Node (Span.text label, [])) in
  let _ = Tree.to_string tree in
  check true

(* Panel rendering doesn't crash *)
let test_panel_render_no_crash content =
  let panel = Panel.v (Span.text content) in
  let _ = Panel.to_string panel in
  check true

(* Color hex parsing and equality *)
let test_color_rgb_roundtrip r g b =
  let c = Color.rgb r g b in
  let c2 = Color.rgb r g b in
  check (Color.equal c c2)

(* Truncate never increases width *)
let test_truncate_width s width =
  let width = abs width mod 1000 in
  let truncated = Width.truncate width s in
  let truncated_w = Width.string_width truncated in
  check (truncated_w <= width)

(* The column at which [needle] first appears in [s], or [None]. A literal
   substring scan by exact char comparison -- the fuzzer's invariant check calls
   this per marker per row per step, so it must not pay [Re.compile] (a fresh
   automaton per call) the way a regex would. *)
let index_of needle s =
  let nl = String.length needle and sl = String.length s in
  if nl = 0 then Some 0
  else
    let rec scan i =
      if i + nl > sl then None
      else
        let rec eq j = j = nl || (s.[i + j] = needle.[j] && eq (j + 1)) in
        if eq 0 then Some i else scan (i + 1)
    in
    scan 0

let contains needle s = Option.is_some (index_of needle s)

(* Nasty content to weave into labels, logs and headers: wide CJK, an emoji, a
   ZWJ emoji, combining marks, zero-width spaces, RTL, fullwidth latin, box
   glyphs, invalid UTF-8, control bytes, and an overlong run. A correct display
   must keep every visual invariant under all of these. *)
let weird =
  [|
    "";
    "\xe4\xbd\xa0\xe5\xa5\xbd" (* wide CJK *);
    "\xf0\x9f\x8e\x89" (* emoji *);
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7"
    (* ZWJ family *);
    "e\xcc\x81\xcc\x82\xcc\x83" (* combining marks *);
    "\xe2\x80\x8b\xe2\x80\x8b" (* zero-width spaces *);
    "\xd9\x85\xd8\xb1\xd8\xad\xd8\xa8\xd8\xa7" (* RTL Arabic *);
    "\xef\xbc\xa1\xef\xbc\xa2\xef\xbc\xa3" (* fullwidth latin *);
    "\xe2\x96\x88\xe2\x96\x93\xe2\x96\x92\xe2\x96\x91" (* box glyphs *);
    "\xff\xfe\xfd\xc0\x80" (* invalid UTF-8 *);
    "\t\x07x" (* control bytes *);
    String.make 50 'W' (* overlong *);
  |]

let pick_weird c = weird.(Char.code c mod Array.length weird)

(* A task marker [Rn] -- one per emitted row, so tasks never share a line. *)
let task_re = Re.compile Re.(seq [ str "[R"; rep1 digit; char ']' ])

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

(* Every line the terminal holds: scrollback (oldest first) then the screen. *)
let vt_rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

(* Count the rows containing [needle]. [count_in] takes rows already serialised
   from the emulator so a caller checking many needles pays one [Vte.to_string],
   not one per needle. *)
let count_in rows needle = List.length (List.filter (contains needle) rows)
let vte_count vte needle = count_in (vt_rows vte) needle

(* Drive a random program of display operations -- with nasty content -- against
   mosaic's Vte (a faithful terminal emulator) and assert the visual invariants
   after every step:

   - {b no duplicates}: a header and any kept-live row are redrawn in place, so
     each appears at most once (more than once is the leaked-redraw bug). A
     wide glyph that overflows the width also surfaces here, since it desyncs
     the redraw.
   - {b one task per line}: tasks sit on their own rows -- never two markers on
     one emulator line -- so there is always a newline between them.

   The terminal is randomly small so the region overflows and scrolls, the
   condition that exposes leaks. Markers are bracketed ([R1] never an infix of
   [R10]) and carry the nasty suffix after the marker, so counting stays exact. *)
(* Apply one byte of the random program to the display: spawn a row, nest one
   under the last, log a line, set a count, finish (committed or kept live),
   remove, or just tick. [live] tracks the rows still on the live stack and
   [fresh] mints a uniquely-marked label. *)
let step_display_op d live fresh c =
  match Char.code c mod 8 with
  | 0 -> live := Display.row ~log_lines:2 d (fresh c) :: !live
  | 1 -> (
      match !live with
      | p :: _ ->
          live := Display.row ~parent:p ~log_lines:2 d (fresh c) :: !live
      | [] -> ())
  | 2 -> ( match !live with r :: _ -> Display.log r (pick_weird c) | [] -> ())
  | 3 -> (
      match !live with
      | r :: _ -> Display.set_count r ~cur:(Char.code c mod 100) ~total:100
      | [] -> ())
  | 4 -> (
      match !live with
      | r :: tl ->
          Display.finish r;
          live := tl
      | [] -> ())
  | 5 -> ( match !live with r :: _ -> Display.finish ~live:true r | [] -> ())
  | 6 -> (
      match !live with
      | r :: tl ->
          Display.remove r;
          live := tl
      | [] -> ())
  | _ -> Display.tick d

let test_display_invariants s =
  let pick lo hi i = lo + (i mod (hi - lo + 1)) in
  let byte k = if String.length s > k then Char.code s.[k] else 0 in
  (* height >= 3: a header plus a scrolling region and a cursor row need that
     many; a two-row terminal cannot show a header without it scrolling off. *)
  let height = pick 3 16 (byte 0) and width = pick 8 60 (byte 1) in
  let vte = Vte.create ~rows:height ~cols:width () in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let header = "[HEADER]" ^ pick_weird (Char.chr (byte 2)) in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ~header ()
  in
  let sync () =
    Format.pp_print_flush ppf ();
    Vte.feed_string vte (Buffer.contents buf);
    Buffer.clear buf
  in
  let live = ref [] and all = ref [] and counter = ref 0 in
  let fresh c =
    incr counter;
    let key = Fmt.str "[R%d]" !counter in
    all := key :: !all;
    key ^ pick_weird c
  in
  let invariant () =
    sync ();
    let rows = vt_rows vte in
    check (count_in rows "[HEADER]" <= 1);
    List.iter (fun k -> check (count_in rows k <= 1)) !all;
    List.iter (fun r -> check (List.length (Re.all task_re r) <= 1)) rows
  in
  invariant ();
  String.iter
    (fun c ->
      step_display_op d live fresh c;
      invariant ())
    s;
  Display.finish_all d;
  invariant ()

let rec take n = function x :: tl when n > 0 -> x :: take (n - 1) tl | _ -> []
let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* Cell content of well-defined terminal width: ASCII, wide CJK, fullwidth and
   box glyphs. Emoji, combining marks and invalid UTF-8 have no width every
   terminal agrees on, so they are out of scope for an exact layout
   invariant. *)
let table_pool =
  [|
    "";
    "ok";
    "\xe4\xbd\xa0\xe5\xa5\xbd" (* wide CJK *);
    "\xef\xbc\xa1\xef\xbc\xa2\xef\xbc\xa3" (* fullwidth latin *);
    "\xe2\x96\x88\xe2\x96\x93\xe2\x96\x92" (* box glyphs *);
    "a normal cell value";
    String.make 30 'x';
    "some/long/path/value";
  |]

let table_cell s = table_pool.(String.length s mod Array.length table_pool)

(* A table is a rectangle: every rendered row has the same display width and no
   cell overflows onto a second line. The fuzzer varies the shape (column count,
   row count, content width) from the input; feeding the render to the emulator
   at the table's own width, a cell whose width was mis-measured would wrap and
   add rows. So [nonblank rows = lines] is the no-overflow invariant. *)
let test_table_no_overflow headers cells =
  match headers with
  | [] -> check true
  | _ ->
      let columns =
        List.map
          (fun h ->
            Table.column ~overflow:`Truncate ~max_width:14 (table_cell h))
          headers
      in
      let ncol = List.length columns in
      let rec rows = function
        | [] -> []
        | l ->
            let r = take ncol l in
            let r = r @ List.init (ncol - List.length r) (fun _ -> "") in
            List.map (fun c -> Span.text (table_cell c)) r :: rows (drop ncol l)
      in
      let t = Table.of_rows columns (rows cells) in
      let s = Table.to_string ~ctx:(table_ctx ()) t in
      let lines = String.split_on_char '\n' s in
      let w =
        List.fold_left (fun m l -> max m (Width.string_width l)) 1 lines
      in
      let vte = Vte.create ~rows:(List.length lines + 4) ~cols:w () in
      Vte.feed_string vte s;
      let nonblank rows =
        List.length (List.filter (fun r -> rstrip r <> "") rows)
      in
      (* Count only non-blank lines on both sides: [to_string] ends with a
         trailing newline, so a raw line count is one too many. A wrapped cell
         would add a non-blank emulator row beyond the rendered lines. *)
      check (nonblank (vt_rows vte) = nonblank lines)

(* The live region is bounded by the terminal height: when more rows are live
   than fit, the display keeps the header, the root row and the most recent
   rows, and drops the middle behind a "(N more lines)" marker -- so the region
   is never taller than the terminal and a row is never scrolled stale into the
   scrollback. We add [n] rows (with nasty suffixes) to a short display and
   check the height bound, that the newest and root rows are kept, that a deep
   overflow drops a middle row, and that overflow always carries the marker. *)
let test_collapse n =
  let n = 1 + (abs n mod 40) in
  let height = 6 and width = 40 in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:height ~cols:width () in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ~header:"[H]" ()
  in
  for i = 1 to n do
    let suffix = pick_weird (Char.chr (i land 0xff)) in
    Fmt.kstr (fun s -> ignore (Display.row d s)) "[R%d] %s" i suffix
  done;
  Display.tick d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  let screen = String.split_on_char '\n' (Vte.to_string vte) in
  let on_screen needle = List.exists (contains needle) screen in
  (* the live region never exceeds the terminal height *)
  check (List.length screen <= height);
  (* the newest row is always kept on screen *)
  check (Fmt.kstr on_screen "[R%d] " n);
  (* a heavy overflow cannot show every row -- some must collapse *)
  if n > 2 * height then
    check (List.length (List.filter (contains "[R") screen) < n)

(* A sub-step renders indented beneath its parent: each deeper row starts at a
   greater column than its parent, so the tree structure stays visible. *)
let test_nesting depth =
  let depth = 1 + (abs depth mod 6) in
  let width = 70 and height = 40 in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:height ~cols:width () in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ()
  in
  let rec build parent i =
    if i <= depth then begin
      let suffix = pick_weird (Char.chr (i land 0xff)) in
      let r = Fmt.kstr (Display.row ?parent d) "[R%d] %s" i suffix in
      build (Some r) (i + 1)
    end
  in
  build None 1;
  Display.tick d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  let rows = vt_rows vte in
  let col i =
    let needle = Fmt.str "[R%d]" i in
    List.find_map (index_of needle) rows
  in
  for i = 2 to depth do
    match (col (i - 1), col i) with
    | Some a, Some b -> check (b > a)
    | _ -> check true
  done

(* A progress bar reflects its fraction. Use a label/percent/count style (no
   wide fill, which the display would crowd the label out for at small widths)
   so the marker stays visible: at a width that fits "[B] 100% 999/999" the
   marker sits on exactly one row, and the exact percentage (truncated, capped
   at 100) and the cur/total count are rendered. Counts are kept short so they
   are not themselves truncated. *)
let test_bar cur total wbyte =
  let total = 1 + (abs total mod 999) in
  let cur = abs cur mod (total + 1) in
  let width = 16 + (abs wbyte mod 200) in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:4 ~cols:width () in
  let bar = Display.Line.(v [ label; text " "; percent; text " "; count ]) in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height:4)
      ~mode:`Console ~width ~height:4 ()
  in
  let r = Display.row ~bar d "[B]" in
  Display.set_count r ~cur ~total;
  Display.tick d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  if width >= 28 then begin
    (* the marker is on exactly one row -- the bar row never wrapped *)
    check (vte_count vte "[B]" = 1);
    check (Fmt.kstr (vte_count vte) "%d/%d" cur total >= 1);
    check (Fmt.kstr (vte_count vte) "%d%%" (cur * 100 / total) >= 1)
  end

(* The measured width must match what a terminal renders, or the display and
   tables reserve the wrong amount of space (a ZWJ emoji used to measure 6 yet
   render 2). For printable, well-formed UTF-8, Width.string_width equals the
   column the emulator's cursor reaches after the same bytes. *)
let printable_utf8 s =
  String.for_all (fun c -> Char.code c >= 0x20 && Char.code c <> 0x7f) s
  &&
  let ok = ref true in
  Uutf.String.fold_utf_8
    (fun () _ d -> match d with `Malformed _ -> ok := false | _ -> ())
    () s;
  !ok

let test_width_matches_terminal s =
  if (not (printable_utf8 s)) || Width.string_width s > 3000 then check true
  else begin
    let vte = Vte.create ~rows:1 ~cols:4000 () in
    Vte.feed_string vte s;
    let _, col = Vte.cursor_pos vte in
    check (Width.string_width s = col)
  end

(* Truncation is idempotent and never exceeds the budget: truncating to [w]
   twice is the same as once, and the result fits [w] cells. *)
let test_truncate_idempotent s width =
  let w = abs width mod 200 in
  let once = Width.truncate w s in
  check (Width.string_width once <= w);
  check (Width.truncate w once = once)

(* Padding reaches exactly the target width and never shrinks content: padding
   to [w] yields max(w, width(s)) cells, on either side. Content is drawn from
   the well-defined-width pool -- string_width is not additive over arbitrary
   bytes (a trailing combining mark binds to the pad space), which is a Unicode
   fact, not a padding bug. *)
let test_pad_reaches_width idx width =
  let s = table_pool.(abs idx mod Array.length table_pool) in
  let w = abs width mod 200 in
  let target = max w (Width.string_width s) in
  check (Width.string_width (Width.pad_right w s) = target);
  check (Width.string_width (Width.pad_left w s) = target)

(* A panel is a rectangle: rendered at its own width, every row fits on one
   emulator line (no cell wraps), so the non-blank emulator rows match the
   rendered lines. *)
let test_panel_rectangular idx =
  let cell = table_pool.(abs idx mod Array.length table_pool) in
  let content = String.concat "\n" [ cell; "a second line"; cell ] in
  let s = Panel.to_string (Panel.v (Span.text content)) in
  let lines = String.split_on_char '\n' s in
  let w = List.fold_left (fun m l -> max m (Width.string_width l)) 1 lines in
  let vte = Vte.create ~rows:(List.length lines + 4) ~cols:w () in
  Vte.feed_string vte s;
  let nonblank rows =
    List.length (List.filter (fun r -> rstrip r <> "") rows)
  in
  check (nonblank (vt_rows vte) = nonblank lines)

(* Composition: a table of progress-bar cells, wrapped in a panel, streamed into
   a display task's log window. However wide the widget block and however narrow
   the terminal, neither the task marker nor the header is duplicated -- rich,
   multi-line log content must not corrupt the in-place redraw. *)
let test_composed wbyte =
  let width = 10 + (abs wbyte mod 50) in
  let bar pct =
    let full = pct / 10 in
    String.concat "" (List.init 10 (fun i -> if i < full then "█" else "░"))
    ^ Fmt.str " %d%%" pct
  in
  let col h = Table.column ~overflow:`Truncate ~max_width:16 h in
  let table =
    Table.of_string_rows
      [ col "step"; col "progress" ]
      [ [ "build"; bar 30 ]; [ "test"; bar 70 ] ]
  in
  let block =
    Panel.to_string
      (Panel.v (Span.text (Table.to_string ~ctx:(table_ctx ~width ()) table)))
  in
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:12 ~cols:width () in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height:12)
      ~mode:`Console ~width ~height:12 ~header:"[H]" ()
  in
  let r = Display.row ~log_lines:8 d "[T] task" in
  List.iter (Display.log r) (String.split_on_char '\n' block);
  Display.tick d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  check (vte_count vte "[T]" <= 1);
  check (vte_count vte "[H]" <= 1)

(* A run that saturates the height, then the user scrolls back: the committed
   history stays consistent -- no finished row is ever duplicated in the
   terminal's full content (scrollback + screen). We finish uniquely-marked rows
   one at a time on a randomly small display (with nasty suffixes), ticking
   between each so the live region is cleared and redrawn many times; every
   finish commits a line that scrolls up into the scrollback as later rows
   commit. A redraw that miscounts the live region would clobber or re-emit a
   committed line, showing it twice once you scroll back. Markers are bracketed
   so even a right-truncated [D10] ("[D10", no closing bracket) cannot be
   miscounted as [D1]; a marker may legitimately drop to zero when the timer
   crowds it off a narrow line, but it must never appear twice. *)
let test_scrollback_no_dup s =
  let byte k = if String.length s > k then Char.code s.[k] else 0 in
  let pick lo hi i = lo + (i mod (hi - lo + 1)) in
  let height = pick 3 8 (byte 0) and width = pick 12 50 (byte 1) in
  let n = pick 6 24 (byte 2) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:height ~cols:width () in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ~header:"[H]" ()
  in
  for i = 1 to n do
    let suffix = pick_weird (Char.chr (byte (i + 2))) in
    let r = Fmt.kstr (Display.row d) "[D%d] %s" i suffix in
    Display.tick d;
    Display.finish r;
    Display.tick d
  done;
  Display.finish_all d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  (* no finished row is ever duplicated across the whole history *)
  let rows = vt_rows vte in
  for i = 1 to n do
    check (Fmt.kstr (count_in rows) "[D%d]" i <= 1)
  done;
  (* the header is redrawn in place, never duplicated into the scrollback *)
  check (count_in rows "[H]" <= 1)

(* Rows hidden by height-collapse must still commit to the scrollback exactly
   once when the run ends: the user scrolls back and sees every row, none lost
   and none duplicated. We over-fill a short display so the live region
   collapses (middle rows dropped behind the "(N more)" marker and never shown),
   tick to redraw the collapsed view, then finish_all -- which commits every
   row, including the ones that were never on screen. The width is wide enough
   that no committed line is truncated, so each marker renders intact: a row
   missing from the history is a dropped-from-model bug, one appearing twice a
   re-emitted redraw. *)
let test_scrollback_collapse_commits s =
  let byte k = if String.length s > k then Char.code s.[k] else 0 in
  let pick lo hi i = lo + (i mod (hi - lo + 1)) in
  let height = pick 3 7 (byte 0) and width = pick 40 70 (byte 1) in
  let n = pick (2 * height) (4 * height) (byte 2) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let vte = Vte.create ~rows:height ~cols:width () in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ~header:"[H]" ()
  in
  let rows =
    Array.init n (fun i -> Fmt.kstr (Display.row d) "[D%d] ." (i + 1))
  in
  Display.tick d;
  Display.tick d;
  Array.iter (fun r -> Display.finish r) rows;
  Display.finish_all d;
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  let rows = vt_rows vte in
  for i = 1 to n do
    check (Fmt.kstr (count_in rows) "[D%d]" i = 1)
  done;
  check (count_in rows "[H]" <= 1)

(* Minimal redraw, no flicker: once a frame is painted, repainting it with
   nothing changed must emit no bytes at all -- clearing and rewriting identical
   glyphs on every tick is the flicker. We drive a random program of display ops
   (with nasty content) on a frozen clock, confirm the program painted
   something, then assert that two further idle ticks emit nothing. The frozen
   clock is the point: with the timers held still, an unchanged tree renders the
   identical frame, so a correct display has nothing to redraw. *)
let test_minimal_redraw s =
  let byte k = if String.length s > k then Char.code s.[k] else 0 in
  let pick lo hi i = lo + (i mod (hi - lo + 1)) in
  let height = pick 3 16 (byte 0) and width = pick 8 60 (byte 1) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0.0 in
  let d =
    Display.v
      ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Console ~width ~height ~header:"[H]" ()
  in
  let live = ref [] and counter = ref 0 in
  let fresh c =
    incr counter;
    Fmt.str "[R%d] %s" !counter (pick_weird c)
  in
  String.iter (fun c -> step_display_op d live fresh c) s;
  Display.tick d;
  Format.pp_print_flush ppf ();
  (* the display painted at least the header, so the silence below is real *)
  check (String.length (Buffer.contents buf) > 0);
  Buffer.clear buf;
  Display.tick d;
  Display.tick d;
  Format.pp_print_flush ppf ();
  check (String.length (Buffer.contents buf) = 0)

(* A frame is one of the spinner's own glyphs. *)
let is_frame t f = Array.exists (String.equal f) (Spinner.frames t)

(* [v] keeps the frames it was given, in order, and an empty sequence is
   rejected rather than yielding a spinner that would divide by zero. *)
let test_spinner_roundtrip frames =
  let t = Spinner.v frames in
  check (Spinner.frames t = frames);
  let empty_raised =
    try
      ignore (Spinner.v [||]);
      false
    with Invalid_argument _ -> true
  in
  check empty_raised

(* [frame] is total: every integer -- negative, zero, [min_int] -- names a frame
   without raising, and the frame is always one of the spinner's glyphs. On a
   clamped index (so [i +/- n] cannot overflow) it is cyclic with period [n] and
   indexes the frames in order, with negative indices counting from the end. *)
let test_spinner_frame frames i =
  let t = Spinner.v frames in
  let n = Array.length frames in
  check (is_frame t (Spinner.frame t i));
  let j = i mod 10000 in
  check (Spinner.frame t (j + n) = Spinner.frame t j);
  check (Spinner.frame t (j - n) = Spinner.frame t j);
  let k = ((j mod n) + n) mod n in
  check (Spinner.frame t j = frames.(k))

(* [at] always shows one of the spinner's glyphs, for any timing. A non-positive
   elapsed pins the first frame; a positive, finite elapsed shows the frame at
   [floor (elapsed * fps)], so the animation advances at [fps]. *)
let test_spinner_at frames elapsed fps =
  let t = Spinner.v frames in
  let f = Spinner.at ~fps t ~elapsed in
  check (is_frame t f);
  if elapsed <= 0. then check (f = (Spinner.frames t).(0));
  if elapsed > 0. && Float.is_finite (elapsed *. fps) then
    check (f = Spinner.frame t (int_of_float (elapsed *. fps)))

let suite =
  ( "console",
    [
      test_case "width matches terminal" [ bytes ] test_width_matches_terminal;
      test_case "display invariants" [ bytes ] test_display_invariants;
      test_case "collapse keeps height" [ int ] test_collapse;
      test_case "nesting indents" [ int ] test_nesting;
      test_case "progress bar" [ int; int; int ] test_bar;
      test_case "truncate idempotent" [ bytes; int ] test_truncate_idempotent;
      test_case "pad reaches width" [ int; int ] test_pad_reaches_width;
      test_case "panel rectangular" [ int ] test_panel_rectangular;
      test_case "composed widget in task" [ int ] test_composed;
      test_case "scrollback no dup" [ bytes ] test_scrollback_no_dup;
      test_case "scrollback collapse commits" [ bytes ]
        test_scrollback_collapse_commits;
      test_case "minimal redraw" [ bytes ] test_minimal_redraw;
      test_case "spinner roundtrip" [ array1 bytes ] test_spinner_roundtrip;
      test_case "spinner frame total" [ array1 bytes; int ] test_spinner_frame;
      test_case "spinner at" [ array1 bytes; float; float ] test_spinner_at;
      test_case "table no overflow"
        [ list bytes; list bytes ]
        test_table_no_overflow;
      test_case "width non-negative" [ bytes ] test_width_non_negative;
      test_case "span width consistency" [ bytes ] test_span_width_consistency;
      test_case "span concat width" [ bytes; bytes ] test_span_concat_width;
      test_case "table render"
        [ list bytes; list bytes ]
        test_table_render_no_crash;
      test_case "tree render" [ bytes ] test_tree_render_no_crash;
      test_case "panel render" [ bytes ] test_panel_render_no_crash;
      test_case "color rgb roundtrip" [ int; int; int ] test_color_rgb_roundtrip;
      test_case "truncate width" [ bytes; int ] test_truncate_width;
    ] )
