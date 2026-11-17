(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let check digits x want =
  Alcotest.(check string)
    (Fmt.str "%h to %d decimals" x digits)
    want
    (Decimal.string_of_float ~digits x)

(* Each of these is exactly halfway in binary, where glibc and MinGW's C
   library disagree. *)
let test_ties () =
  check 1 4.25 "4.3";
  check 1 0.25 "0.3";
  check 1 (-4.25) "-4.3";
  check 0 11.5 "12";
  check 0 12.5 "13";
  check 2 1.125 "1.13"

let test_digits () =
  check 0 3. "3";
  check 1 3. "3.0";
  check 2 0.05 "0.05";
  check 3 0.0004 "0.000";
  check 1 9.96 "10.0";
  check 2 123.456 "123.46"

let test_zero_has_no_sign () =
  check 1 (-0.04) "0.0";
  check 0 (-0.) "0"

let test_special () =
  check 1 Float.nan "nan";
  check 1 Float.infinity "inf";
  check 1 Float.neg_infinity "-inf";
  check 0 1e20 "100000000000000000000"

let test_negative_digits () =
  Alcotest.check_raises "negative digits"
    (Invalid_argument "Console.Decimal.string_of_float: negative digits")
    (fun () -> ignore (Decimal.string_of_float ~digits:(-1) 1.))

let suite =
  ( "decimal",
    [
      Alcotest.test_case "ties" `Quick test_ties;
      Alcotest.test_case "digits" `Quick test_digits;
      Alcotest.test_case "zero has no sign" `Quick test_zero_has_no_sign;
      Alcotest.test_case "special values" `Quick test_special;
      Alcotest.test_case "negative digits" `Quick test_negative_digits;
    ] )
