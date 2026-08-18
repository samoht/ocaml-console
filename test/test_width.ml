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

let test_malformed_utf8 () =
  Alcotest.(check int) "replacement cell" 1 (Width.string_width "\xff");
  Alcotest.(check string)
    "truncate emits replacement" "\xef\xbf\xbd" (Width.truncate 1 "\xff")

let test_spacing_combining_mark () =
  Alcotest.(check int)
    "spacing combining mark attaches to preceding cell" 1
    (Width.string_width "x\xe0\xa5\x80")

let test_ansi_ignored () =
  let styled = "\027[1mhello\027[0m" in
  let width = Width.string_width styled in
  Alcotest.(check int) "ANSI codes ignored" 5 width

let test_ansi_does_not_break_grapheme () =
  let combining = "\xea\xa8\xb4" in
  Alcotest.(check int)
    "styled cluster has the same width"
    (Width.string_width ("x" ^ combining))
    (Width.string_width ("x\027[31m" ^ combining ^ "\027[0m"))

let test_osc_ignored () =
  let link = "\027]8;;https://example.com\027\\docs\027]8;;\027\\" in
  Alcotest.(check int) "OSC ignored" 4 (Width.string_width link);
  Alcotest.(check string)
    "OSC preserved by truncation"
    "\027]8;;https://example.com\027\\do\027]8;;\027\\" (Width.truncate 2 link)

(* A value cut to fit reads as a value that wraps unless the cut is marked, and
   a mark at one end throws away the other: a hash is identified by its leading
   characters and a path named by its trailing ones. The mark goes in the
   middle, and the result still fits. *)
let digest =
  "sha256:a376ef543f48c9b17ed2b1cbe32b4d1e6a5f0c9d8e7b6a5948372615f0e1d2c3"

let test_ellipsize_keeps_both_ends () =
  let cut = Width.ellipsize 21 digest in
  Alcotest.(check string)
    "the middle is what goes" "sha256:a37\xe2\x80\xa615f0e1d2c3" cut;
  Alcotest.(check int) "and the result fits" 21 (Width.string_width cut)

let test_ellipsize_short_enough () =
  Alcotest.(check string)
    "a value that fits is untouched" "abc" (Width.ellipsize 3 "abc");
  Alcotest.(check string)
    "and so is a shorter one" "abc" (Width.ellipsize 8 "abc")

let test_ellipsize_no_room () =
  Alcotest.(check string)
    "nothing fits in nothing" "" (Width.ellipsize 0 digest);
  Alcotest.(check string)
    "one column says only that it was cut" "\xe2\x80\xa6"
    (Width.ellipsize 1 digest)

let test_ellipsize_ansi_costs_nothing () =
  let styled = "\027[1mabcdefghij\027[0m" in
  let cut = Width.ellipsize 5 styled in
  Alcotest.(check int) "styling occupies no column" 5 (Width.string_width cut);
  Alcotest.(check bool)
    "the style is closed at the cut" true
    (Width.string_width cut = 5 && String.length cut > 5)

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

let test_wrap_uses_terminal_cells () =
  let wide u = if Uchar.to_int u = 0x754c then 2 else 1 in
  Alcotest.(check string)
    "uses terminal cells" "界 a"
    (Width.wrap ~char_width:wide 4 "界 a")

(* Grapheme clusters measure as a terminal renders them, not as the sum of their
   code points: a ZWJ emoji sequence (a family) is one 2-wide cell, not 6.
   Over-counting made the display and tables reserve too much space. *)
let test_zwj_emoji_width () =
  let width = Width.string_width in
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
      Alcotest.test_case "malformed UTF-8" `Quick test_malformed_utf8;
      Alcotest.test_case "spacing combining mark" `Quick
        test_spacing_combining_mark;
      Alcotest.test_case "ANSI ignored" `Quick test_ansi_ignored;
      Alcotest.test_case "ANSI preserves grapheme" `Quick
        test_ansi_does_not_break_grapheme;
      Alcotest.test_case "OSC ignored" `Quick test_osc_ignored;
      Alcotest.test_case "ellipsize keeps both ends" `Quick
        test_ellipsize_keeps_both_ends;
      Alcotest.test_case "ellipsize leaves a short value" `Quick
        test_ellipsize_short_enough;
      Alcotest.test_case "ellipsize with no room" `Quick test_ellipsize_no_room;
      Alcotest.test_case "ellipsize counts no escape" `Quick
        test_ellipsize_ansi_costs_nothing;
      Alcotest.test_case "wrap basic" `Quick test_wrap_basic;
      Alcotest.test_case "wrap with indent" `Quick test_wrap_with_indent;
      Alcotest.test_case "wrap short" `Quick test_wrap_short;
      Alcotest.test_case "wrap single long word" `Quick
        test_wrap_single_long_word;
      Alcotest.test_case "wrap multiline input" `Quick test_wrap_multiline_input;
      Alcotest.test_case "wrap empty" `Quick test_wrap_empty;
      Alcotest.test_case "wrap terminal cells" `Quick
        test_wrap_uses_terminal_cells;
    ] )
