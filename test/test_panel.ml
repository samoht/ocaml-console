(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_basic () =
  let panel = Panel.view (View.text "content") in
  let output = View.to_string panel in
  Alcotest.(check bool) "contains content" true (contains "content" output)

let test_with_title () =
  let panel = Panel.view ~title:(View.text "Title") (View.text "body") in
  let output = View.to_string panel in
  Alcotest.(check bool) "contains title" true (contains "Title" output);
  Alcotest.(check bool) "contains body" true (contains "body" output)

let render renderer panel =
  View_harness.capture @@ fun ppf ->
  Fmt.set_style_renderer ppf renderer;
  View.pp ppf panel

(* A border's gradient grades the frame once round it, still, from the
   first colour at the top-left corner, clockwise and back: on a frame of
   ten cells, the second of two colours is at the fifth cell from it, the
   bottom-right corner, and each cell between blends the two around its
   place, as Color.gradient does. Not the content. *)
let test_gradient_frame () =
  let border =
    Border.with_gradient [ Color.rgb 255 0 0; Color.rgb 0 0 255 ] Border.ascii
  in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Color.set_depth ppf `True_color;
  View.pp ppf (Panel.view ~border ~padding:0 (View.text "ab"));
  Format.pp_print_flush ppf ();
  let cells = Console_text.strip ~keep_styles:false (Buffer.contents buf) in
  Alcotest.(check string) "the glyphs" "+--+\n|ab|\n+--+" cells;
  let out = Buffer.contents buf in
  let fg (r, g, b) glyph = Fmt.str "\027[38;2;%d;%d;%dm%s\027[0m" r g b glyph in
  let line = List.map (fun (rgb, glyph) -> fg rgb glyph) in
  (* Cell by cell, so that a gradient drawn the other way round fails. *)
  Alcotest.(check string)
    "the top edge, from the first colour"
    (String.concat ""
       (line
          [
            ((255, 0, 0), "+");
            ((204, 0, 51), "-");
            ((153, 0, 102), "-");
            ((102, 0, 153), "+");
          ]))
    (List.hd (String.split_on_char '\n' out));
  Alcotest.(check string)
    "the left edge, cell 9, back towards the first; the right edge, cell 4; \
     the content not graded"
    (fg (204, 0, 51) "|" ^ "ab" ^ fg (51, 0, 204) "|")
    (List.nth (String.split_on_char '\n' out) 1);
  Alcotest.(check string)
    "the bottom edge, the second colour at its right corner"
    (String.concat ""
       (line
          [
            ((153, 0, 102), "+");
            ((102, 0, 153), "-");
            ((51, 0, 204), "-");
            ((0, 0, 255), "+");
          ]))
    (List.nth (String.split_on_char '\n' out) 2)

let test_multiline_preserves_styles () =
  let content =
    View.(
      styled Style.(fg Color.red) (text "red")
      ++ text "\n"
      ++ styled Style.(fg Color.blue) (text "blue"))
  in
  let panel = Panel.view content in
  let ansi = render `Ansi_tty panel in
  Alcotest.(check bool) "first line is styled" true (contains "\027[31m" ansi);
  Alcotest.(check bool) "second line is styled" true (contains "\027[34m" ansi);
  Alcotest.(check bool)
    "plain formatter has no escapes" false
    (String.contains (render `None panel) '\027')

let test_invalid_geometry () =
  Alcotest.check_raises "negative padding"
    (Invalid_argument "Console.Panel.view: negative padding") (fun () ->
      ignore (Panel.view ~padding:(-1) (View.text "x")));
  Alcotest.(check string)
    "a room too small cuts the frame" "╭─…\n│ …\n╰─…"
    (View.to_string
       (View.sized ~width:3
          (Panel.view ~padding:2 ~expand:true (View.text "x"))))

let test_fixed_width_truncates () =
  let panel =
    View.sized ~width:8
      (Panel.view ~padding:1 ~expand:true ~title:(View.text "long title")
         (View.text "abcdefgh"))
  in
  let rendered = View.to_string panel |> String.split_on_char '\n' in
  Alcotest.(check int) "three lines" 3 (List.length rendered);
  List.iter
    (fun line ->
      Alcotest.(check int)
        "fixed width" 8
        (Console_width.string_width (Console_text.strip line)))
    rendered;
  Alcotest.(check bool)
    "content was clipped" false
    (contains "abcdefgh" (String.concat "\n" rendered))

let lines = Text_harness.lines
let render_at margin panel = View_harness.printed ~margin panel

(* The formatter's margin is a ceiling on the box: a line wider than the room
   wraps at its words, each continuation hanging under the text after the
   line's first word, and the box is exactly the margin wide. *)
let test_margin_wraps () =
  let panel =
    Panel.view
      (View.text
         "REFUSED main has no baseline and the gate worktree does not exist\nok")
  in
  Alcotest.(check string)
    "wrapped box"
    (String.concat "\n"
       [
         "╭────────────────────────────╮";
         "│ REFUSED main has no        │";
         "│         baseline and the   │";
         "│         gate worktree does │";
         "│         not exist          │";
         "│ ok                         │";
         "╰────────────────────────────╯";
       ])
    (render_at 30 panel)

(* A box that fits keeps its natural width, and [to_string] is natural. *)
let test_margin_natural () =
  Alcotest.(check string)
    "fits" "╭────╮\n│ ok │\n╰────╯"
    (render_at 30 (Panel.view (View.text "ok")));
  let long = String.make 100 'x' in
  Alcotest.(check int)
    "to_string is natural" 104
    (Console_width.string_width
       (List.hd (lines (View.to_string (Panel.view (View.text long))))))

(* A styled border opens one sequence for each run of border glyphs. *)
let test_styled_border () =
  let red = Color.rgb 255 0 0 in
  let panel =
    Panel.view ~padding:0
      ~border:(Border.with_style (Style.fg red) Border.single)
      (View.text "x")
  in
  let r = "\027[38;2;255;0;0m" and reset = "\027[0m" in
  Alcotest.(check string)
    "runs"
    (String.concat ""
       [
         r;
         "\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x90";
         reset;
         "\n";
         r;
         "\xe2\x94\x82";
         reset;
         "x";
         r;
         "\xe2\x94\x82";
         reset;
         "\n";
         r;
         "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x98";
         reset;
       ])
    (View.to_string ~styled:true panel)

let test_wide_border () =
  let border = View_harness.frame "\u{754c}" in
  let out =
    View.to_string
      (View.sized ~width:12 (Panel.view ~border ~expand:true (View.text "x")))
  in
  let widths =
    List.map (fun l -> View.width (View.text l)) (String.split_on_char '\n' out)
  in
  Alcotest.(check (list int))
    "all edges respect fixed width" [ 12; 12; 12 ] widths

let test_to_string_width () =
  List.iter
    (fun line ->
      Alcotest.(check bool)
        ("within 12 cells: " ^ line)
        true
        (View.width (View.text line) <= 12))
    (String.split_on_char '\n'
       (View.to_string
          (View.sized ~width:12
             (Panel.view (View.text "a line longer than twelve cells")))))

let test_overflowing_padding () =
  Alcotest.check_raises "no line can hold this padding"
    (Invalid_argument "Console.Panel.view: padding too large") (fun () ->
      ignore (Panel.view ~padding:max_int (View.text "x")))

let test_title_follows_state () =
  let name = ref "one" in
  let label = View.fit (fun _ -> View.text !name) in
  let panel = Panel.view ~title:label ~subtitle:label (View.text "body") in
  let first = View.to_string panel in
  name := "two";
  let second = View.to_string panel in
  Alcotest.(check bool) "first title" true (contains "one" first);
  Alcotest.(check bool) "title follows" true (contains " two " second);
  Alcotest.(check bool) "old title gone" false (contains "one" second)

let suite =
  ( "panel",
    [
      Alcotest.test_case "a fit title follows the state" `Quick
        test_title_follows_state;
      Alcotest.test_case "a gradient frame" `Quick test_gradient_frame;
      Alcotest.test_case "huge padding is refused before drawing" `Quick
        test_overflowing_padding;
      Alcotest.test_case "to_string within a width" `Quick test_to_string_width;
      Alcotest.test_case "two-cell border respects fixed width" `Quick
        test_wide_border;
      Alcotest.test_case "styled border" `Quick test_styled_border;
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "with title" `Quick test_with_title;
      Alcotest.test_case "multiline styles" `Quick
        test_multiline_preserves_styles;
      Alcotest.test_case "invalid geometry" `Quick test_invalid_geometry;
      Alcotest.test_case "fixed width truncates" `Quick
        test_fixed_width_truncates;
      Alcotest.test_case "margin wraps" `Quick test_margin_wraps;
      Alcotest.test_case "margin natural" `Quick test_margin_natural;
    ] )
