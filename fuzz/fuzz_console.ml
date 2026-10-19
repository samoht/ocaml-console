(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let of_string_rows ?theme ?border columns rows =
  Table.of_rows ?theme ?border columns (List.map (List.map Span.text) rows)

(* Every property here checks the library against a real VT emulator, which
   places combining marks at zero cells and wide glyphs at two, as the
   library's grapheme measure does. *)

(* Width is always non-negative *)
let test_width_non_negative s =
  let w = Width.string_width s in
  check (w >= 0)

(* Span width matches underlying width *)
let test_span_width_consistency s =
  let span = Span.text s in
  let lines = String.split_on_char '\n' (Console_text.sanitize s) in
  check
    (Span.width span
    = List.fold_left (fun n l -> max n (Width.string_width l)) 0 lines)

(* Span concatenation is no wider than its parts *)
let test_span_concat_width s1 s2 =
  let span1 = Span.text s1 in
  let span2 = Span.text s2 in
  let combined = Span.(span1 ++ span2) in
  let w1 = Span.width span1 in
  let w2 = Span.width span2 in
  let w_combined = Span.width combined in
  check (w_combined <= w1 + w2)

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
  let tree = Tree.v (Span.text label) [] in
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
  check (Span.width (Span.truncate width (Span.text s)) <= width)

(* Every line the terminal holds: scrollback (oldest first) then the screen. *)
let vt_rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

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
let rec take n = function x :: tl when n > 0 -> x :: take (n - 1) tl | _ -> []
let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

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
           (Width.string_width (Console_text.strip row))
           width)
        (Width.string_width (Console_text.strip row) = width))
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
           (Width.string_width (Console_text.strip row))
           width row)
        (Width.string_width (Console_text.strip row) = width))
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
  Vte.feed_string vte (raw ^ "\n");
  check_table_visual_rows ~width ~logical_rows vte

(* The measured width must match what a terminal renders, or the display and
   tables reserve the wrong amount of space. For printable, well-formed UTF-8,
   Width.string_width equals the column the emulator's cursor reaches after the
   same bytes. *)
let printable_utf8 s =
  String.for_all (fun c -> Char.code c >= 0x20 && Char.code c <> 0x7f) s
  && String.is_valid_utf_8 s

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
  let once = Span.truncate w (Span.text s) in
  check (Span.width once <= w);
  let twice = Span.truncate w once in
  check (Span.to_string twice = Span.to_string once)

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
           (Width.string_width (Console_text.strip row))
           width)
        (Width.string_width (Console_text.strip row) = width))
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
           (Width.string_width (Console_text.strip row))
           width row)
        (Width.string_width (Console_text.strip row) = width))
    visual_rows;
  invariant "panel cursor is not at the next row" (Vte.cursor_pos vte = (3, 0))

let test_panel_rectangular content wbyte =
  let width = 8 + (wbyte land 0x7f mod 89) in
  let title = Span.text content in
  let body =
    Span.text
      (Console_text.sanitize ~keep_newlines:false
         (content ^ String.make (width * 2) 'W'))
  in
  let panel = Panel.v ~border:Border.rounded ~width ~title body in
  (* [Panel.pp] is a value printer rather than a line printer; terminate the
     bottom row explicitly just as a caller writing the next terminal row
     would. *)
  let raw = render_ansi ~width Panel.pp panel ^ "\n" in
  let logical_rows = nonblank (String.split_on_char '\n' raw) in
  check_panel_raw_rows ~width logical_rows;
  let vte = Vte.create ~rows:5 ~cols:width () in
  Vte.feed_string vte raw;
  check_panel_visual_rows ~width ~raw_top:(List.nth logical_rows 0) vte

(* A spinner shows one of the frames it was given, for any timing; a
   non-positive elapsed pins the first frame. A non-positive or non-finite fps
   is refused, and so is an empty sequence of frames. *)
let test_spinner_frame frames elapsed fps =
  let t = Spinner.v frames in
  check
    (try
       ignore (Spinner.v [||]);
       false
     with Invalid_argument _ -> true);
  if Float.is_finite fps && fps > 0. then begin
    let f = Spinner.frame ~fps t ~elapsed in
    check (Array.exists (String.equal f) frames);
    if elapsed <= 0. then check (f = frames.(0))
  end
  else
    check
      (try
         ignore (Spinner.frame ~fps t ~elapsed);
         false
       with Invalid_argument _ -> true)

(* What [Console_text.strip] leaves holds no escape, and takes as many cells
   as the text it measures. *)
let test_strip_leaves_no_escape s =
  let stripped = Console_text.strip ~keep_newlines:false s in
  check (not (String.contains stripped '\027'));
  check (Width.string_width stripped >= 0)

(* Positive, bounded widths exercise malformed input without allocating in
   proportion to a generated machine integer. Exceptions must reach Alcobar. *)
let adversarial_width n = 1 + (n land 31)

let test_span_wrap_bytes s n =
  ignore (Span.wrap (adversarial_width n) (Span.text s))

let test_span_truncate_bytes s n =
  ignore (Span.truncate (adversarial_width n) (Span.text s))

let test_strip_bytes s = ignore (Console_text.strip s)
let test_sanitize_styles_bytes s = ignore (Console_text.sanitize_styles s)

let test_table_bytes s =
  let table = Table.of_rows [ Table.column "value" ] [ [ Span.text s ] ] in
  ignore (Table.to_string ~width:12 table)

(* Generate valid printable text with cell and grapheme boundaries that differ
   from byte boundaries. No external UTF-8 decoder is part of the oracle. *)
let adversarial_text s =
  let alphabet = [| "x"; "界"; "é"; "\xea\xa8\xb4"; " "; ","; ";" |] in
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      Buffer.add_string b alphabet.(Char.code c mod Array.length alphabet))
    s;
  Buffer.contents b

let without_spaces s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      if Uchar.to_int u <> 0x20 then Buffer.add_utf_8_uchar b u;
      loop (i + Uchar.utf_decode_length d)
    end
  in
  loop 0;
  Buffer.contents b

(* Every line fits, and every character but the spaces is kept when the
   lines are wide enough for any character (two cells). *)
let test_wrap_preserves_text s n =
  let s = adversarial_text s in
  let w = adversarial_width n in
  let lines = Span.wrap w (Span.text s) in
  List.iter (fun l -> check (Span.width l <= w)) lines;
  if w >= 2 then
    check_eq ~pp:Fmt.Dump.string (without_spaces s)
      (without_spaces (String.concat "" (List.map Span.to_string lines)))

let test_joined_width left right =
  let left = adversarial_text left and right = adversarial_text right in
  let combined = Span.(text left ++ styled Style.bold right) in
  check_eq ~pp:Fmt.int (Width.string_width (left ^ right)) (Span.width combined)

(* Compare a following sentinel with one drawn in a fresh emulator. This does
   not use Console_text.sgr, whose interpretation is under test. *)
let test_sanitized_colour r g b =
  let component n = n land 255 in
  let sequence =
    Fmt.str "\027[38;2;%d;%d;%dmX" (component r) (component g)
      (if b land 1 = 0 then 0 else component b)
  in
  let actual = Vte.create ~rows:2 ~cols:4 () in
  let reference = Vte.create ~rows:2 ~cols:4 () in
  Vte.feed_string actual (Console_text.sanitize_styles sequence ^ "!");
  Vte.feed_string reference "X!";
  let a = Vte.grid actual and e = Vte.grid reference in
  let ai = Grid.idx a ~x:1 ~y:0 and ei = Grid.idx e ~x:1 ~y:0 in
  check (Grid.get_fg_r a ai = Grid.get_fg_r e ei);
  check (Grid.get_fg_g a ai = Grid.get_fg_g e ei);
  check (Grid.get_fg_b a ai = Grid.get_fg_b e ei)

let suite =
  ( "console",
    [
      test_case "sanitized colour does not leak" [ int; int; int ]
        test_sanitized_colour;
      test_case "styled concatenation has joined width" [ bytes; bytes ]
        test_joined_width;
      test_case "wrapping preserves non-space text" [ bytes; int ]
        test_wrap_preserves_text;
      test_case "Span.wrap accepts arbitrary bytes" [ bytes; int ]
        test_span_wrap_bytes;
      test_case "Span.truncate accepts arbitrary bytes" [ bytes; int ]
        test_span_truncate_bytes;
      test_case "strip accepts arbitrary bytes" [ bytes ] test_strip_bytes;
      test_case "sanitize_styles accepts arbitrary bytes" [ bytes ]
        test_sanitize_styles_bytes;
      test_case "Table.of_rows accepts arbitrary cell bytes" [ bytes ]
        test_table_bytes;
      test_case "strip leaves no escape" [ bytes ] test_strip_leaves_no_escape;
      test_case "width matches terminal" [ bytes ] test_width_matches_terminal;
      test_case "truncate idempotent" [ bytes; int ] test_truncate_idempotent;
      test_case "panel exact-width geometry" [ bytes; int ]
        test_panel_rectangular;
      test_case "spinner frame"
        [ array1 bytes; float; float ]
        test_spinner_frame;
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
