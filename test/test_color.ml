(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_hex_6digit () =
  let color = Color.hex "#FF5500" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "6-digit hex" true (Color.equal color expected)

let test_hex_3digit () =
  let color = Color.hex "#F50" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "3-digit hex" true (Color.equal color expected)

let test_hex_no_hash () =
  let color = Color.hex "FF5500" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "hex without #" true (Color.equal color expected)

let test_predefined () =
  Alcotest.(check bool) "red equals red" true (Color.equal Color.red Color.red);
  Alcotest.(check bool)
    "red not equal blue" false
    (Color.equal Color.red Color.blue)

let suite =
  ( "color",
    [
      Alcotest.test_case "hex 6-digit" `Quick test_hex_6digit;
      Alcotest.test_case "hex 3-digit" `Quick test_hex_3digit;
      Alcotest.test_case "hex no hash" `Quick test_hex_no_hash;
      Alcotest.test_case "predefined" `Quick test_predefined;
    ] )
