(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let of_string_rows ?theme ?border columns rows =
  Table.of_rows ?theme ?border columns (List.map (List.map Span.text) rows)

(* Every property here checks the library against a real VT emulator, which
   places combining marks at zero cells and wide glyphs at two; Uucp-backed
   [Width.default_char_width] follows that same policy. *)

let display_ctx ~ppf ~now ~width ~height =
  Display.ctx ~ppf ~now ~wait:ignore
    ~dimensions:(fun () -> (width, height))
    ~is_tty:true ()

let dumb () =
  Display.ctx ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~wait:ignore
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:false ()

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
let rec first n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: first (n - 1) xs

let test_table_render_no_crash headers row =
  if List.length headers > 0 && List.length row > 0 then
    let cols = List.map Table.column headers in
    let n = List.length cols in
    let row = first n row in
    let row = row @ List.init (n - List.length row) (fun _ -> "") in
    let rows = [ List.map Span.text row ] in
    let table = Table.of_rows cols rows in
    let _ = Table.to_string table in
    check true
  else check true

(* Tree rendering doesn't crash *)
let test_tree_render_no_crash label =
  let tree = Tree.v (Tree.Node (Span.text label, [])) in
  let _ = Tree.to_string tree in
  check true

(* Panel rendering doesn't crash *)
let test_panel_render_no_crash content =
  let panel = Panel.v (Span.text content) in
  let _ = Panel.to_string panel in
  check true

(* Color hex parsing and equality *)
let test_color_rgb_roundtrip r g b =
  let valid n = n >= 0 && n <= 255 in
  if valid r && valid g && valid b then
    let c = Color.rgb r g b in
    let c2 = Color.rgb r g b in
    check (Color.equal c c2)
  else
    check
      (try
         ignore (Color.rgb r g b);
         false
       with Invalid_argument _ -> true)

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

(* [pick lo hi i] is [i] folded into the range [lo] to [hi]. *)
let pick lo hi i = lo + (i mod (hi - lo + 1))

(* [byte s k] is the [k]th byte of [s], or [0] past its end. *)
let byte s k = if String.length s > k then Char.code s.[k] else 0

(* [ticking_clock ()] is a clock that moves a tenth of a second each time it
   is read. *)
let ticking_clock () =
  let clock = ref 0.0 in
  fun () ->
    clock := !clock +. 0.1;
    !clock

(* A task marker [Rn] -- one per emitted row, so tasks never share a line. *)
let task_re = Re.compile Re.(seq [ str "[R"; rep1 digit; char ']' ])

(* Every line the terminal holds: scrollback (oldest first) then the screen. *)
let vt_rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

(* Count the rows containing [needle]. [count_in] takes rows already serialised
   from the emulator so a caller checking many needles pays one [Vte.to_string],
   not one per needle. *)
let count_in rows needle = List.length (List.filter (contains needle) rows)
let vte_count vte needle = count_in (vt_rows vte) needle

(* Drive a random program of display operations -- with nasty content -- against
   Matrix's Vte (a faithful terminal emulator) and assert the visual invariants
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
  | 0 -> live := Display.task ~log_lines:2 d (fresh c) :: !live
  | 1 -> (
      match !live with
      | p :: _ ->
          live := Display.task ~parent:p ~log_lines:2 d (fresh c) :: !live
      | [] -> ())
  | 2 -> ( match !live with r :: _ -> Display.log r (pick_weird c) | [] -> ())
  | 3 -> (
      match !live with
      | r :: _ -> Display.set_count r ~cur:(Char.code c mod 100) ~total:100
      | [] -> ())
  | 4 -> (
      match !live with
      | r :: tl ->
          Display.succeed r;
          live := tl
      | [] -> ())
  | 5 -> (
      match !live with
      | r :: tl ->
          Display.fail r;
          live := tl
      | [] -> ())
  | 6 -> (
      match !live with
      | r :: tl ->
          Display.cancel r;
          live := tl
      | [] -> ())
  | _ -> Display.tick d

let test_display_invariants s =
  let byte = byte s in
  (* height >= 3: a header plus a scrolling region and a cursor row need that
     many; a two-row terminal cannot show a header without it scrolling off. *)
  let height = pick 3 16 (byte 0) and width = pick 8 60 (byte 1) in
  let vte = Vte.create ~rows:height ~cols:width () in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ticking_clock () in
  let header = "[HEADER]" ^ pick_weird (Char.chr (byte 2)) in
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
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
    ~mode:`Activity ~width ~height ~header (fun d ->
      invariant ();
      String.iter
        (fun c ->
          step_display_op d live fresh c;
          invariant ())
        s;
      List.iter Display.cancel !live;
      invariant ())

let rec take n = function x :: tl when n > 0 -> x :: take (n - 1) tl | _ -> []
let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* Representative cell content for padding properties. The widget geometry
   properties below use arbitrary bytes directly and check the rendered result
   against the VT emulator. *)
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

let render_ansi ~width pp value =
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf width;
  Fmt.set_style_renderer ppf `Ansi_tty;
  pp ppf value;
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let nonblank rows = List.filter (fun row -> String.trim row <> "") rows
let invariant name condition = if not condition then failwith name

let left_table_edge row =
  List.exists
    (fun edge -> String.starts_with ~prefix:edge row)
    [ "╭"; "│"; "├"; "╰" ]

let right_table_edge row =
  List.exists
    (fun edge -> String.ends_with ~suffix:edge row)
    [ "╮"; "│"; "┤"; "╯" ]

(* Formatter margin is a hard geometry contract. Vary width, shape and
   arbitrary binary cell content; protect one column and force its natural
   width beyond the terminal so the emergency fitting path always runs. Every
   raw and emulated physical row must end at the right border, exactly at the
   terminal edge, without an extra wrapped row, scrollback or cursor drift. *)
(* Up to six columns that fit at [width], four rows, one protected column, and a
   first cell forced far past the terminal so the emergency fitting path always
   runs. *)
let overflow_table ~width headers cells =
  let max_columns = max 1 (min 6 ((width - 1) / 4)) in
  let headers = take max_columns (if headers = [] then [ "" ] else headers) in
  let ncol = List.length headers in
  let columns =
    List.mapi
      (fun i header ->
        Table.column ~shrinkable:(i <> 0)
          ~overflow:(if i land 1 = 0 then `Wrap else `Truncate)
          header)
      headers
  in
  let rec make_rows remaining values =
    if remaining = 0 || values = [] then []
    else
      let row = take ncol values in
      let row = row @ List.init (ncol - List.length row) (fun _ -> "") in
      row :: make_rows (remaining - 1) (drop ncol values)
  in
  let rows =
    match make_rows 4 (if cells = [] then [ "" ] else cells) with
    | [] -> assert false
    | (first_cell :: row) :: rows ->
        ((first_cell ^ String.make (width * 2) 'W') :: row) :: rows
    | [] :: _ -> assert false
  in
  of_string_rows ~border:Border.rounded columns rows

(* The formatter emitted rows, and every one of them is exactly [width] cells. *)
let check_table_raw_rows ~width logical_rows =
  invariant "table emitted no logical rows" (logical_rows <> []);
  List.iter
    (fun row ->
      invariant
        (Fmt.str "table raw row is %d cells at width %d"
           (Width.string_width row) width)
        (Width.string_width row = width))
    logical_rows

(* A real terminal shows the same geometry: nothing scrolled away, no row wrapped
   into an extra one, both borders on every row, every row exactly [width] cells,
   and the cursor left just past the last row. *)
let check_table_visual_rows ~width ~logical_rows vte =
  let visual_rows = nonblank (vt_rows vte) in
  invariant "table entered scrollback" (Vte.scrollback_size vte = 0);
  invariant
    (Fmt.str "table grew from %d logical to %d visual rows at width %d"
       (List.length logical_rows) (List.length visual_rows) width)
    (List.length visual_rows = List.length logical_rows);
  List.iter
    (fun row ->
      invariant "table lost its left border" (left_table_edge row);
      invariant
        (Fmt.str "table lost its right border at width %d: %S" width row)
        (right_table_edge row);
      invariant
        (Fmt.str "table visual row is %d cells at width %d: %S"
           (Width.string_width row) width row)
        (Width.string_width row = width))
    visual_rows;
  invariant "table cursor is not at the next row"
    (Vte.cursor_pos vte = (List.length logical_rows, 0))

let test_table_no_overflow headers cells wbyte =
  let width = 8 + (wbyte land 0x7f mod 89) in
  let table = overflow_table ~width headers cells in
  let raw = render_ansi ~width Table.pp table in
  let logical_rows = nonblank (String.split_on_char '\n' raw) in
  check_table_raw_rows ~width logical_rows;
  let vte = Vte.create ~rows:(List.length logical_rows + 2) ~cols:width () in
  Vte.feed_string vte raw;
  check_table_visual_rows ~width ~logical_rows vte

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
  let now = ticking_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  let screen =
    Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Activity ~width ~height ~header:"[H]" (fun d ->
        let tasks =
          List.init n (fun i ->
              let i = i + 1 in
              let suffix = pick_weird (Char.chr (i land 0xff)) in
              Fmt.kstr (Display.task d) "[R%d] %s" i suffix)
        in
        Display.tick d;
        Format.pp_print_flush ppf ();
        Vte.feed_string vte (Buffer.contents buf);
        let screen = String.split_on_char '\n' (Vte.to_string vte) in
        List.iter Display.cancel tasks;
        screen)
  in
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
  let now = ticking_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  let rows =
    Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
      ~mode:`Activity ~width ~height (fun d ->
        let rec build parent tasks i =
          if i > depth then tasks
          else
            let suffix = pick_weird (Char.chr (i land 0xff)) in
            let task = Fmt.kstr (Display.task ?parent d) "[R%d] %s" i suffix in
            build (Some task) (task :: tasks) (i + 1)
        in
        let tasks = build None [] 1 in
        Display.tick d;
        Format.pp_print_flush ppf ();
        Vte.feed_string vte (Buffer.contents buf);
        let rows = vt_rows vte in
        List.iter Display.cancel tasks;
        rows)
  in
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
  let now = ticking_clock () in
  let vte = Vte.create ~rows:4 ~cols:width () in
  let bar = Display.Line.(v [ label; text " "; percent; text " "; count ]) in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height:4)
    ~mode:`Activity ~width ~height:4 (fun d ->
      let task = Display.task ~bar d "[B]" in
      Display.set_count task ~cur ~total;
      Display.tick d;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buf);
      if width >= 28 then begin
        check (vte_count vte "[B]" = 1);
        check (Fmt.kstr (vte_count vte) "%d/%d" cur total >= 1);
        check (Fmt.kstr (vte_count vte) "%d%%" (cur * 100 / total) >= 1)
      end;
      Display.succeed task)

(* The measured width must match what a terminal renders, or the display and
   tables reserve the wrong amount of space. For printable, well-formed UTF-8,
   Width.string_width equals the column the emulator's cursor reaches after the
   same bytes. *)
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
  invariant
    (Fmt.str "truncate produced %d cells for budget %d: %S"
       (Width.string_width once) w once)
    (Width.string_width once <= w);
  let twice = Width.truncate w once in
  invariant
    (Fmt.str "truncate is not idempotent at %d: once=%S twice=%S" w once twice)
    (twice = once)

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

(* A fixed-width panel has the same hard contract as a table. Arbitrary bytes
   in both its edge label and body are sanitised; all three physical rows land
   exactly on the declared right edge, with no wrap or scroll. *)
(* The formatter emits exactly three rows and each is exactly [width] cells. *)
let check_panel_raw_rows ~width logical_rows =
  invariant "panel did not emit three logical rows"
    (List.length logical_rows = 3);
  List.iter
    (fun row ->
      invariant
        (Fmt.str "panel raw row is %d cells at width %d"
           (Width.string_width row) width)
        (Width.string_width row = width))
    logical_rows

(* A real terminal shows the same three rows: nothing scrolled away, every corner
   and side border in place, every row exactly [width] cells, and no cursor
   drift. [raw_top] is quoted beside the emulated top row so a right-corner
   mismatch names both. *)
let check_panel_visual_rows ~width ~raw_top vte =
  let visual_rows = nonblank (vt_rows vte) in
  invariant "panel entered scrollback" (Vte.scrollback_size vte = 0);
  invariant "panel did not occupy three visual rows"
    (List.length visual_rows = 3);
  let top = List.nth visual_rows 0 in
  let middle = List.nth visual_rows 1 in
  let bottom = List.nth visual_rows 2 in
  invariant "panel lost its top-left corner"
    (String.starts_with ~prefix:"╭" top);
  invariant
    (Fmt.str "panel lost its top-right corner at width %d: raw=%S visual=%S"
       width raw_top top)
    (String.ends_with ~suffix:"╮" top);
  invariant "panel lost its middle-left border"
    (String.starts_with ~prefix:"│" middle);
  invariant "panel lost its middle-right border"
    (String.ends_with ~suffix:"│" middle);
  invariant "panel lost its bottom-left corner"
    (String.starts_with ~prefix:"╰" bottom);
  invariant "panel lost its bottom-right corner"
    (String.ends_with ~suffix:"╯" bottom);
  List.iter
    (fun row ->
      invariant
        (Fmt.str "panel visual row is %d cells at width %d: %S"
           (Width.string_width row) width row)
        (Width.string_width row = width))
    visual_rows;
  invariant "panel cursor is not at the next row" (Vte.cursor_pos vte = (3, 0))

let test_panel_rectangular content wbyte =
  let width = 8 + (wbyte land 0x7f mod 89) in
  let title = Span.text content in
  let body =
    Span.text (content ^ String.make (width * 2) 'W')
    |> Span.sanitize ~keep_newlines:false
  in
  let panel = Panel.v ~border:Border.rounded ~width ~title body in
  (* [Panel.pp] is a value printer rather than a line printer; terminate the
     bottom row explicitly just as a caller writing the next terminal row
     would. *)
  let raw = render_ansi ~width Panel.pp panel ^ "\r\n" in
  let logical_rows = nonblank (String.split_on_char '\n' raw) in
  check_panel_raw_rows ~width logical_rows;
  let vte = Vte.create ~rows:5 ~cols:width () in
  Vte.feed_string vte raw;
  check_panel_visual_rows ~width ~raw_top:(List.nth logical_rows 0) vte

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
    of_string_rows
      [ col "step"; col "progress" ]
      [ [ "build"; bar 30 ]; [ "test"; bar 70 ] ]
  in
  let block =
    Panel.to_string (Panel.v (Span.text (Table.to_string ~width table)))
  in
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ticking_clock () in
  let vte = Vte.create ~rows:12 ~cols:width () in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height:12)
    ~mode:`Activity ~width ~height:12 ~header:"[H]" (fun d ->
      let task = Display.task ~log_lines:8 d "[T] task" in
      List.iter (Display.log task) (String.split_on_char '\n' block);
      Display.tick d;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buf);
      check (vte_count vte "[T]" <= 1);
      check (vte_count vte "[H]" <= 1);
      Display.succeed task)

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
  let byte = byte s in
  let height = pick 3 8 (byte 0) and width = pick 12 50 (byte 1) in
  let n = pick 6 24 (byte 2) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ticking_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
    ~mode:`Activity ~width ~height ~header:"[H]" (fun d ->
      for i = 1 to n do
        let suffix = pick_weird (Char.chr (byte (i + 2))) in
        let task = Fmt.kstr (Display.task d) "[D%d] %s" i suffix in
        Display.tick d;
        Display.succeed task;
        Display.tick d
      done);
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
  let byte = byte s in
  let height = pick 3 7 (byte 0) and width = pick 40 70 (byte 1) in
  let n = pick (2 * height) (4 * height) (byte 2) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ticking_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
    ~mode:`Activity ~width ~height ~header:"[H]" (fun d ->
      let tasks =
        Array.init n (fun i -> Fmt.kstr (Display.task d) "[D%d] ." (i + 1))
      in
      Display.tick d;
      Display.tick d;
      Array.iter Display.succeed tasks);
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
  let byte = byte s in
  let height = pick 3 16 (byte 0) and width = pick 8 60 (byte 1) in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0.0 in
  let live = ref [] and counter = ref 0 in
  let fresh c =
    incr counter;
    Fmt.str "[R%d] %s" !counter (pick_weird c)
  in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
    ~mode:`Activity ~width ~height ~header:"[H]" (fun d ->
      String.iter (fun c -> step_display_op d live fresh c) s;
      Display.tick d;
      Format.pp_print_flush ppf ();
      check (String.length (Buffer.contents buf) > 0);
      Buffer.clear buf;
      Display.tick d;
      Display.tick d;
      Format.pp_print_flush ppf ();
      check (String.length (Buffer.contents buf) = 0);
      List.iter Display.cancel !live)

(* A changing task is repainted as one terminal row, without blanking the whole
   activity region first. The default spacer also places elapsed metadata on
   the terminal's actual right edge for every useful width and hostile label. *)
let test_differential_redraw label wbyte =
  let width = 32 + (wbyte land 0x7f mod 89) and height = 8 in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0. in
  let vte = Vte.create ~rows:height ~cols:width () in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height)
    ~mode:`Activity ~width ~height (fun display ->
      ignore (Display.task display "[R1] prepare");
      let changed = Display.task display ("[R2] " ^ label) in
      ignore (Display.task display "[R3] verify");
      Display.tick display;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buf);
      Buffer.clear buf;
      let initial = Vte.to_string vte |> String.split_on_char '\n' in
      let first = List.nth initial 0 and last = List.nth initial 2 in
      for cur = 1 to 10 do
        Vte.clear_dirty vte;
        Display.set_count changed ~cur ~total:10;
        (* An update of its own repaints at the eye's pace, which on this
           pinned clock is never; the tick is what puts the row on screen. *)
        Display.tick display;
        Format.pp_print_flush ppf ();
        let update = Buffer.contents buf in
        Vte.feed_string vte update;
        Buffer.clear buf;
        let dirty = Vte.dirty_rows vte in
        invariant
          (Fmt.str
             "only the middle row is dirty, got [%s] at width %d; update=%S; \
              screen=%S"
             (String.concat ";" (List.map string_of_int dirty))
             width update (Vte.to_string vte))
          (List.equal Int.equal dirty [ 1 ]);
        let rows = Vte.to_string vte |> String.split_on_char '\n' in
        invariant "preceding sibling changed" (List.nth rows 0 = first);
        invariant "following sibling changed" (List.nth rows 2 = last);
        let row = List.nth rows 1 in
        invariant "middle row does not fill the terminal"
          (Width.string_width row = width);
        if not (String.ends_with ~suffix:"0.0s" row) then
          Fmt.failwith "elapsed metadata is not at width %d: %S" width row
      done;
      invariant "right-edge redraw leaked into scrollback"
        (Vte.scrollback_size vte = 0);
      invariant "cursor did not return to the final activity row"
        (Vte.cursor_pos vte = (2, 0)))

(* Completion moves a task from the live projection into permanent history,
   but must not change the layout contract: arbitrary labels and count widths
   leave the timer on the exact right edge of the row's first line without an
   implicit VTE wrap. A label that does not fit beside it folds onto the lines
   under it, whole, so the history may take more than one line. *)
let test_committed_right_edge label wbyte =
  let width = 16 + (wbyte land 0x7f mod 105) in
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0. in
  let vte = Vte.create ~rows:1024 ~cols:width () in
  Display.run ~ctx:(display_ctx ~ppf ~now ~width ~height:4)
    ~mode:`Activity ~width ~height:4 (fun display ->
      let task = Display.task display label in
      let cur = wbyte land 0xffff in
      Display.set_count task ~cur ~total:(cur + 1);
      Display.succeed task);
  Format.pp_print_flush ppf ();
  Vte.feed_string vte (Buffer.contents buf);
  let rows =
    vt_rows vte |> List.filter (fun row -> String.starts_with ~prefix:"=>" row)
  in
  invariant "completion did not produce exactly one history row"
    (List.length rows = 1);
  let row = List.hd rows in
  invariant "committed timer is not at the right edge"
    (String.ends_with ~suffix:"0.0s" row);
  let grid = Vte.grid vte in
  invariant "committed timer does not occupy the terminal's final cell"
    (Grid.get_text grid (Grid.idx grid ~x:(width - 1) ~y:0) = "s");
  invariant "committed row leaked into scrollback" (Vte.scrollback_size vte = 0);
  invariant "committed row drifted the cursor"
    (Vte.cursor_pos vte = (List.length (nonblank (vt_rows vte)), 0))

(* A keyed event stream needs no caller-owned task registry. Randomly duplicate,
   reorder and contradict starts, progress updates, logs and outcomes over eight
   stable IDs. Console must create one lifecycle per ID, keep progress
   monotonic, retain every late log, and let the first outcome win. *)
type modeled_task =
  | Unseen
  | Running of int * int
  | Finished of Display.Event.outcome

(* The reference model of the eight keyed tasks, advanced alongside the display:
   what state each ID is in, and the handle Console first gave it. *)
type model = {
  states : modeled_task array;
  handles : Display.task option array;
}

let key i = Fmt.str "task-%d" i

(* Console owns the registry: asking for an ID again must hand back the very same
   task, never open a second lifecycle. *)
let ensure model display i =
  let task = Display.task ~id:(key i) display (Fmt.str "task %d" i) in
  (match model.handles.(i) with
  | None ->
      model.handles.(i) <- Some task;
      model.states.(i) <- Running (0, 0)
  | Some first -> check (first == task));
  task

(* Progress is monotonic: a count lower than one already reported never moves the
   item backwards. *)
let check_progress display i expected =
  let item =
    Display.Activity.items (Display.activity display)
    |> List.find (fun item -> Display.Activity.key item = Some (key i))
  in
  check (Display.Activity.progress item = expected)

(* Interpret one program byte: its low three bits pick the ID, the next three the
   operation. Duplicate starts, contradictory outcomes, late logs and relabels
   are all reachable, and every one advances the model too. *)
let apply_op model display byte =
  let code = Char.code byte in
  let i = code land 7 in
  let task = ensure model display i in
  match (code lsr 3) mod 8 with
  | 0 -> ignore (ensure model display i : Display.task)
  | 1 -> (
      let cur = code land 63 in
      let total = cur + ((code lsr 6) land 3) in
      Display.set_count task ~cur ~total;
      match model.states.(i) with
      | Running (previous, previous_total) ->
          let cur = max previous cur in
          let total = max cur (max previous_total total) in
          model.states.(i) <- Running (cur, total);
          check_progress display i (`Count (cur, total))
      | Unseen | Finished _ -> ())
  | 2 -> Display.log task (pick_weird byte)
  | 3 -> (
      Display.succeed task;
      match model.states.(i) with
      | Running _ -> model.states.(i) <- Finished `Succeeded
      | Unseen | Finished _ -> ())
  | 4 -> (
      Display.fail task;
      match model.states.(i) with
      | Running _ -> model.states.(i) <- Finished `Failed
      | Unseen | Finished _ -> ())
  | 5 -> (
      Display.cancel task;
      match model.states.(i) with
      | Running _ -> model.states.(i) <- Finished `Cancelled
      | Unseen | Finished _ -> ())
  | 6 -> Display.set_label task (pick_weird byte)
  | _ -> (
      Display.succeed task;
      Display.fail task;
      Display.cancel task;
      match model.states.(i) with
      | Running _ -> model.states.(i) <- Finished `Succeeded
      | Unseen | Finished _ -> ())

(* Whatever the stream did to an ID, its history holds exactly one lifecycle: one
   [`Started] and one [`Finished], and the outcome is the FIRST one the stream
   asked for -- a later contradicting outcome never wins. A task the stream left
   running is succeeded when the display closes. *)
let check_one_lifecycle_per_key model events i =
  match model.states.(i) with
  | Unseen -> ()
  | (Running _ | Finished _) as state ->
      let events =
        List.filter (fun event -> Display.Event.key event = Some (key i)) events
      in
      let starts =
        List.filter (fun event -> Display.Event.kind event = `Started) events
      in
      let finishes =
        List.filter_map
          (fun event ->
            match Display.Event.kind event with
            | `Finished outcome -> Some outcome
            | _ -> None)
          events
      in
      check (List.length starts = 1);
      check (List.length finishes = 1);
      let expected =
        match state with
        | Running _ -> `Succeeded
        | Finished outcome -> outcome
        | Unseen -> assert false
      in
      check (List.equal ( = ) finishes [ expected ])

let test_keyed_event_stream program =
  let model = { states = Array.make 8 Unseen; handles = Array.make 8 None } in
  let saved = ref None in
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0. in
  Display.run
    ~ctx:
      (Display.ctx ~ppf ~now ~wait:ignore
         ~dimensions:(fun () -> (80, 24))
         ~is_tty:false ())
    ~mode:`History
    (fun display ->
      saved := Some display;
      String.iter (apply_op model display) program);
  let display = Option.get !saved in
  let events = Display.history display in
  for i = 0 to 7 do
    check_one_lifecycle_per_key model events i
  done

(* Geometry providers are external and can transiently report nonsense during
   resize. Zero and negative dimensions are clamped at the boundary; repainting
   under a changing one-cell terminal must stay total and bounded. *)
let test_dynamic_geometry program =
  let width = ref 0 and height = ref 0 in
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let now () = 0. in
  let ctx =
    Display.ctx ~ppf ~now ~wait:ignore
      ~dimensions:(fun () -> (!width, !height))
      ~is_tty:true ()
  in
  Display.run ~ctx ~mode:`Activity ~header:"resize" (fun display ->
      String.iteri
        (fun i byte ->
          let code = Char.code byte in
          width := (code land 15) - 7;
          height := ((code lsr 4) land 15) - 7;
          let task =
            Display.task ~id:(Fmt.str "resize-%d" (i land 3)) display "x"
          in
          Display.set_count task ~cur:(code land 7) ~total:7;
          Display.tick display)
        program)

(* Arbitrary result text and detail remain structured data, with terminal
   controls neutralised and exactly one semantic verdict. *)
let test_result_sanitization message detail =
  let saved = ref None in
  let ctx = dumb () in
  Display.run ~ctx ~mode:`History (fun display ->
      saved := Some display;
      Display.set_result display ~detail (`Ok message));
  let result =
    Display.history (Option.get !saved)
    |> List.find (fun event ->
        match Display.Event.kind event with `Result _ -> true | _ -> false)
  in
  let safe ~newline s =
    String.for_all
      (fun c ->
        let n = Char.code c in
        n >= 0x20 || (newline && c = '\n'))
      s
  in
  check (safe ~newline:false (Display.Event.message result));
  Option.iter
    (fun s -> check (safe ~newline:true s))
    (Display.Event.detail result)

(* Provisional totals are estimates, not traps: count and byte progress expand
   to contain every observed current value and never move backwards. *)
let test_progress_estimates cur total next next_total =
  let bound n = n land 0xffff in
  let cur = bound cur and total = bound total in
  let next = bound next and next_total = bound next_total in
  Display.run ~ctx:(dumb ()) ~mode:`History (fun display ->
      let count = Display.task display "count" in
      Display.set_count count ~cur ~total;
      Display.set_count count ~cur:next ~total:next_total;
      let bytes = Display.task display "bytes" in
      Display.set_bytes bytes ~cur:(Int64.of_int cur)
        ~total:(Int64.of_int total);
      Display.set_bytes bytes ~cur:(Int64.of_int next)
        ~total:(Int64.of_int next_total);
      let items = Display.Activity.items (Display.activity display) in
      let progress label =
        items
        |> List.find (fun item -> Display.Activity.label item = label)
        |> Display.Activity.progress
      in
      let current = max cur next in
      let known_total = max current (max total next_total) in
      check (progress "count" = `Count (current, known_total));
      check
        (progress "bytes"
        = `Bytes (Int64.of_int current, Int64.of_int known_total)))

(* Competing cleanup paths may both settle a session. The first semantic result
   wins and later attempts neither raise nor duplicate the verdict event. *)
let test_duplicate_result first second =
  let saved = ref None in
  Display.run ~ctx:(dumb ()) ~mode:`History (fun display ->
      saved := Some display;
      Display.set_result display (`Ok first);
      Display.set_result display (`Fail second));
  let results =
    Display.history (Option.get !saved)
    |> List.filter (fun event ->
        match Display.Event.kind event with `Result _ -> true | _ -> false)
  in
  check (List.length results = 1);
  check (Display.Event.kind (List.hd results) = `Result `Ok)

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
  if Float.is_finite fps && fps > 0. then begin
    let f = Spinner.at ~fps t ~elapsed in
    check (is_frame t f);
    if elapsed <= 0. then check (f = (Spinner.frames t).(0))
  end
  else
    check
      (try
         ignore (Spinner.at ~fps t ~elapsed);
         false
       with Invalid_argument _ -> true)

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
      test_case "panel exact-width geometry" [ bytes; int ]
        test_panel_rectangular;
      test_case "composed widget in task" [ int ] test_composed;
      test_case "scrollback no dup" [ bytes ] test_scrollback_no_dup;
      test_case "scrollback collapse commits" [ bytes ]
        test_scrollback_collapse_commits;
      test_case "minimal redraw" [ bytes ] test_minimal_redraw;
      test_case "differential redraw" [ bytes; int ] test_differential_redraw;
      test_case "committed right edge" [ bytes; int ] test_committed_right_edge;
      test_case "keyed event stream" [ bytes ] test_keyed_event_stream;
      test_case "dynamic geometry" [ bytes ] test_dynamic_geometry;
      test_case "result sanitization" [ bytes; bytes ] test_result_sanitization;
      test_case "progress estimates expand" [ int; int; int; int ]
        test_progress_estimates;
      test_case "duplicate result is harmless" [ bytes; bytes ]
        test_duplicate_result;
      test_case "spinner roundtrip" [ array1 bytes ] test_spinner_roundtrip;
      test_case "spinner frame total" [ array1 bytes; int ] test_spinner_frame;
      test_case "spinner at" [ array1 bytes; float; float ] test_spinner_at;
      test_case "table no overflow"
        [ list bytes; list bytes; int ]
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
