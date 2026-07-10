(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
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

let suite =
  ( "style",
    [
      Alcotest.test_case "none" `Quick test_none;
      Alcotest.test_case "bold" `Quick test_bold;
      Alcotest.test_case "composition" `Quick test_composition;
    ] )
