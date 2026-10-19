(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let at width rule =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf width;
  Rule.pp ppf rule;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_plain () =
  Alcotest.(check string) "unlabelled" "-----" (at 5 (Rule.v ~glyph:"-" ()))

let test_label () =
  let label = Span.styled Style.bold "Build" in
  Alcotest.(check string)
    "center" "-- Build --"
    (at 11 (Rule.v ~glyph:"-" ~label ()));
  Alcotest.(check string)
    "left" "Build -----"
    (at 11 (Rule.v ~glyph:"-" ~align:`Left ~label ()));
  Alcotest.(check string)
    "right" "----- Build"
    (at 11 (Rule.v ~glyph:"-" ~align:`Right ~label ()))

(* A label wider than the line is cut to it, not refused. *)
let test_long_label_is_cut () =
  Alcotest.(check string)
    "cut to the margin" "lo…"
    (at 3 (Rule.v ~label:(Span.text "long") ()))

let test_invalid () =
  Alcotest.check_raises "wide glyph"
    (Invalid_argument "Console.Rule.v: glyph must occupy one cell") (fun () ->
      ignore (Rule.v ~glyph:"--" ()));
  Alcotest.check_raises "a control is a space"
    (Invalid_argument "Console.Rule.v: glyph must occupy one cell") (fun () ->
      ignore (Rule.v ~glyph:"-\027" ()))

let suite =
  ( "rule",
    [
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "label" `Quick test_label;
      Alcotest.test_case "long label is cut" `Quick test_long_label_is_cut;
      Alcotest.test_case "invalid" `Quick test_invalid;
    ] )
