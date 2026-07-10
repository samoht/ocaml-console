(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_ascii () =
  let width = Width.string_width "hello" in
  Alcotest.(check int) "ASCII width" 5 width

let test_empty () =
  let width = Width.string_width "" in
  Alcotest.(check int) "empty width" 0 width

let test_ansi_ignored () =
  let styled = "\027[1mhello\027[0m" in
  let width = Width.string_width styled in
  Alcotest.(check int) "ANSI codes ignored" 5 width

let test_wrap_basic () =
  let result = Width.wrap 20 "hello world foo bar baz" in
  Alcotest.(check string) "wraps at width" "hello world foo bar\nbaz" result

let test_wrap_with_indent () =
  let result = Width.wrap ~indent:2 20 "hello world foo bar baz" in
  Alcotest.(check string)
    "wraps with indent" "  hello world foo\n  bar baz" result

let test_wrap_short () =
  let result = Width.wrap 80 "short text" in
  Alcotest.(check string) "no wrapping needed" "short text" result

let test_wrap_single_long_word () =
  let result = Width.wrap 5 "superlongword next" in
  Alcotest.(check string) "long word on own line" "superlongword\nnext" result

let test_wrap_multiline_input () =
  let result = Width.wrap 20 "line one\nline two\nline three" in
  Alcotest.(check string)
    "normalizes newlines" "line one line two\nline three" result

let test_wrap_empty () =
  let result = Width.wrap 20 "" in
  Alcotest.(check string) "empty string" "" result

(* Grapheme clusters measure as a terminal renders them, not as the sum of their
   code points: a ZWJ emoji sequence (a family) is one 2-wide cell, not 6.
   Over-counting made the display and tables reserve too much space. The library
   leaves wide widths to its caller, so the test installs the Uucp-backed
   hint. *)
let uucp_char_width u =
  match Uucp.Break.tty_width_hint u with -1 -> 0 | n -> n

let test_zwj_emoji_width () =
  let width = Width.string_width ~char_width:uucp_char_width in
  Alcotest.(check int)
    "ZWJ family is 2 wide" 2
    (width
       "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7");
  Alcotest.(check int) "plain emoji is 2 wide" 2 (width "\xf0\x9f\x8e\x89");
  Alcotest.(check int) "combining mark adds nothing" 1 (width "e\xcc\x81")

let suite =
  ( "width",
    [
      Alcotest.test_case "ASCII" `Quick test_ascii;
      Alcotest.test_case "zwj emoji width" `Quick test_zwj_emoji_width;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "ANSI ignored" `Quick test_ansi_ignored;
      Alcotest.test_case "wrap basic" `Quick test_wrap_basic;
      Alcotest.test_case "wrap with indent" `Quick test_wrap_with_indent;
      Alcotest.test_case "wrap short" `Quick test_wrap_short;
      Alcotest.test_case "wrap single long word" `Quick
        test_wrap_single_long_word;
      Alcotest.test_case "wrap multiline input" `Quick test_wrap_multiline_input;
      Alcotest.test_case "wrap empty" `Quick test_wrap_empty;
    ] )
