(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_text () =
  let span = Span.text "hello" in
  Alcotest.(check int) "span width" 5 (Span.width span)

let test_concat () =
  let span = Span.(text "hello" ++ text " world") in
  Alcotest.(check int) "concat width" 11 (Span.width span)

let test_plain () =
  let span = Span.styled Style.bold "hello" in
  let plain = Span.to_plain_string span in
  Alcotest.(check string) "plain string" "hello" plain

(* [anim] is the still animation: every frame is [to_string], so a span lays out
   in an animated Layout next to moving widgets. *)
let test_anim () =
  let span = Span.text "hello" in
  let a = Span.anim span in
  Alcotest.(check string)
    "frame matches to_string" (Span.to_string span)
    (Anim.frame a ~elapsed:2.0)

let suite =
  ( "span",
    [
      Alcotest.test_case "text" `Quick test_text;
      Alcotest.test_case "concat" `Quick test_concat;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "anim" `Quick test_anim;
    ] )
