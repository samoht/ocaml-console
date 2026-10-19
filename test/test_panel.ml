(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains pattern str = Re.execp (Re.compile (Re.str pattern)) str

let test_basic () =
  let panel = Panel.v (Span.text "content") in
  let output = Panel.to_string panel in
  Alcotest.(check bool) "contains content" true (contains "content" output)

let test_with_title () =
  let panel = Panel.v ~title:(Span.text "Title") (Span.text "body") in
  let output = Panel.to_string panel in
  Alcotest.(check bool) "contains title" true (contains "Title" output);
  Alcotest.(check bool) "contains body" true (contains "body" output)

let render renderer panel =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  Panel.pp ppf panel;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_multiline_preserves_styles () =
  let content =
    Span.(
      styled Style.(fg Color.red) "red"
      ++ text "\n"
      ++ styled Style.(fg Color.blue) "blue")
  in
  let panel = Panel.v content in
  let ansi = render `Ansi_tty panel in
  Alcotest.(check bool) "first line is styled" true (contains "\027[31m" ansi);
  Alcotest.(check bool) "second line is styled" true (contains "\027[34m" ansi);
  Alcotest.(check bool)
    "plain formatter has no escapes" false
    (String.contains (render `None panel) '\027')

let test_invalid_geometry () =
  Alcotest.check_raises "negative padding"
    (Invalid_argument "Console.Panel.v: negative padding") (fun () ->
      ignore (Panel.v ~padding:(-1) (Span.text "x")));
  Alcotest.check_raises "width too small"
    (Invalid_argument "Console.Panel.v: width too small for padding") (fun () ->
      ignore (Panel.v ~padding:2 ~width:5 (Span.text "x")))

let test_fixed_width_truncates () =
  let panel =
    Panel.v ~padding:1 ~width:8 ~title:(Span.text "long title")
      (Span.text "abcdefgh")
  in
  let rendered = Panel.to_string panel |> String.split_on_char '\n' in
  Alcotest.(check int) "three lines" 3 (List.length rendered);
  List.iter
    (fun line -> Alcotest.(check int) "fixed width" 8 (Width.string_width line))
    rendered;
  Alcotest.(check bool)
    "content was clipped" false
    (contains "abcdefgh" (String.concat "\n" rendered))

let lines s = String.split_on_char '\n' s

let render_at margin panel =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Panel.pp ppf panel;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The formatter's margin is a ceiling on the box: a line wider than the room
   wraps at its words, each continuation hanging under the text after the
   line's first word, and the box is exactly the margin wide. *)
let test_margin_wraps () =
  let panel =
    Panel.v
      (Span.text
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
    (render_at 30 (Panel.v (Span.text "ok")));
  let long = String.make 100 'x' in
  Alcotest.(check int)
    "to_string is natural" 104
    (Width.string_width
       (List.hd (lines (Panel.to_string (Panel.v (Span.text long))))))

(* A gradient border takes its colour on each border cell; a run of border
   glyphs opens one sequence and recolours only where the colour changes. *)
let test_gradient_border () =
  let red = Color.rgb 255 0 0 and blue = Color.rgb 0 0 255 in
  let across = Gradient.v ~length:1 [ red; blue ] in
  let panel g =
    Panel.v ~padding:0
      ~border:(Border.with_style (Style.fg_gradient g) Border.single)
      (Span.text "x")
  in
  let r = "\027[38;2;255;0;0m" and b = "\027[38;2;0;0;255m" in
  let reset = "\027[0m" in
  Alcotest.(check string)
    "across"
    (String.concat ""
       [
         r;
         "\xe2\x94\x8c";
         b;
         "\xe2\x94\x80\xe2\x94\x90";
         reset;
         "\n";
         r;
         "\xe2\x94\x82";
         reset;
         "x";
         b;
         "\xe2\x94\x82";
         reset;
         "\n";
         r;
         "\xe2\x94\x94";
         b;
         "\xe2\x94\x80\xe2\x94\x98";
         reset;
       ])
    (Panel.to_string ~styled:true (panel across));
  let down = Gradient.v ~direction:(0, 1) ~length:2 [ red; blue ] in
  let p = "\027[38;2;127;0;128m" in
  Alcotest.(check string)
    "down"
    (String.concat ""
       [
         r;
         "\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x90";
         reset;
         "\n";
         p;
         "\xe2\x94\x82";
         reset;
         "x";
         p;
         "\xe2\x94\x82";
         reset;
         "\n";
         b;
         "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x98";
         reset;
       ])
    (Panel.to_string ~styled:true (panel down))

let suite =
  ( "panel",
    [
      Alcotest.test_case "gradient border" `Quick test_gradient_border;
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
