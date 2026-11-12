(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let check_parts ~branch ~last ~pipe ~space guide =
  Alcotest.(check string) "branch" branch (Guide.branch guide);
  Alcotest.(check string) "last" last (Guide.last guide);
  Alcotest.(check string) "pipe" pipe (Guide.pipe guide);
  Alcotest.(check string) "space" space (Guide.space guide)

let test_presets () =
  check_parts ~branch:"+-- " ~last:"+-- " ~pipe:"|   " ~space:"    " Guide.ascii;
  check_parts ~branch:"├── " ~last:"└── " ~pipe:"│   " ~space:"    "
    Guide.single

let test_v_keeps_printable_glyphs () =
  let guide = Guide.v ~branch:"" ~last:"x" ~pipe:"🧪" ~space:"  " () in
  check_parts ~branch:"" ~last:"x" ~pipe:"🧪" ~space:"  " guide

(* A control character in a glyph is drawn as a space, so a tree keeps one
   line for each node and its lines their width. *)
let test_v_draws_controls_as_spaces () =
  let guide =
    Guide.v ~branch:"a\nb" ~last:"\027[31m" ~pipe:"\t" ~space:"\000" ()
  in
  check_parts ~branch:"a b" ~last:" [31m" ~pipe:" " ~space:" " guide;
  let tree =
    Tree.v (View.text "r")
      [
        Tree.v (View.text "a") [ Tree.v (View.text "x") [] ];
        Tree.v (View.text "b") [];
      ]
  in
  let lines =
    String.split_on_char '\n' (View.to_string (Tree.view ~guide tree))
  in
  Alcotest.(check (list string))
    "one line for each node"
    [ "r"; "a ba"; "  [31mx"; " [31mb" ]
    lines

let test_pp_quotes_glyphs () =
  let guide = Guide.v ~branch:"\"" ~last:"\\" ~pipe:"|" ~space:" " () in
  Alcotest.(check string)
    "diagnostic form"
    "{ branch = \"\\\"\"; last = \"\\\\\"; pipe = \"|\"; space = \" \" }"
    (Fmt.str "%a" Guide.pp guide)

let suite =
  ( "guide",
    [
      Alcotest.test_case "presets" `Quick test_presets;
      Alcotest.test_case "v keeps printable glyphs" `Quick
        test_v_keeps_printable_glyphs;
      Alcotest.test_case "v draws a control as a space" `Quick
        test_v_draws_controls_as_spaces;
      Alcotest.test_case "pp quotes the glyphs" `Quick test_pp_quotes_glyphs;
    ] )
