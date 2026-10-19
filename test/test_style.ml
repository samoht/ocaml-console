(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_none () =
  let ansi = Style.to_ansi Style.none in
  Alcotest.(check string) "none produces empty" "" ansi

let test_bold () =
  let ansi = Style.to_ansi Style.bold in
  Alcotest.(check string) "bold ANSI code" "\027[1m" ansi

let test_composition () =
  let style = Style.(bold + fg Color.red + underline) in
  Alcotest.(check bool) "composed style not none" false (Style.is_none style)

let red = Color.rgb 255 0 0
let blue = Color.rgb 0 0 255

let test_colours () =
  let s = Style.(bold + fg red + bg blue) in
  Alcotest.(check (option string))
    "foreground" (Some "#ff0000")
    (Option.map (Fmt.str "%a" Color.pp) (Style.foreground s));
  Alcotest.(check string)
    "to_ansi" "\027[1;38;2;255;0;0;48;2;0;0;255m" (Style.to_ansi s);
  Alcotest.(check bool)
    "the right colour wins" true
    (Style.equal (Style.fg blue) Style.(fg red + fg blue))

let test_shows_on_space () =
  let check label expected s =
    Alcotest.(check bool) label expected (Style.shows_on_space s)
  in
  check "foreground" false Style.(bold + faint + italic + blink + fg red);
  check "background" true (Style.bg red);
  check "reverse" true Style.reverse;
  check "underline" true Style.underline;
  check "strikethrough" true Style.strikethrough

let suite =
  ( "style",
    [
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "shows on space" `Quick test_shows_on_space;
      Alcotest.test_case "none" `Quick test_none;
      Alcotest.test_case "bold" `Quick test_bold;
      Alcotest.test_case "composition" `Quick test_composition;
    ] )
