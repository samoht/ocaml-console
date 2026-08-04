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

let test_rgb_range () =
  let rejected label rgb =
    Alcotest.check_raises label
      (Invalid_argument "Console.Color.rgb: component outside 0..255") rgb
  in
  rejected "negative component" (fun () -> ignore (Color.rgb (-1) 0 0));
  rejected "component above 255" (fun () -> ignore (Color.rgb 0 0 256))

let test_invalid_constructors () =
  Alcotest.check_raises "malformed hex"
    (Invalid_argument "Console.Color.hex: expected 3 or 6 hex digits")
    (fun () -> ignore (Color.hex "1234"));
  Alcotest.check_raises "invalid hex digit"
    (Invalid_argument "Console.Color.hex: invalid hex character") (fun () ->
      ignore (Color.hex "12x"));
  Alcotest.check_raises "palette below range"
    (Invalid_argument "Console.Color.palette: index outside 0..255") (fun () ->
      ignore (Color.palette (-1)));
  Alcotest.check_raises "palette above range"
    (Invalid_argument "Console.Color.palette: index outside 0..255") (fun () ->
      ignore (Color.palette 256))

let test_codes () =
  let check = Alcotest.(check string) in
  check "red foreground" "31" (Color.to_fg_code Color.red);
  check "bright cyan background" "106" (Color.to_bg_code Color.bright_cyan);
  check "RGB foreground" "38;2;1;2;3" (Color.to_fg_code (Color.rgb 1 2 3));
  check "palette background" "48;5;42" (Color.to_bg_code (Color.palette 42))

let suite =
  ( "color",
    [
      Alcotest.test_case "hex 6-digit" `Quick test_hex_6digit;
      Alcotest.test_case "hex 3-digit" `Quick test_hex_3digit;
      Alcotest.test_case "hex no hash" `Quick test_hex_no_hash;
      Alcotest.test_case "predefined" `Quick test_predefined;
      Alcotest.test_case "rgb range" `Quick test_rgb_range;
      Alcotest.test_case "invalid constructors" `Quick test_invalid_constructors;
      Alcotest.test_case "ANSI codes" `Quick test_codes;
    ] )
