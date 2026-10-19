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
   the mark is ASCII, as git's diffstat writes it. A hash is identified by both
   of its ends, so the mark goes in the middle and the result still fits. *)
let digest =
  "sha256:a376ef543f48c9b17ed2b1cbe32b4d1e6a5f0c9d8e7b6a5948372615f0e1d2c3"

let test_ellipsize_keeps_both_ends () =
  let cut = Width.ellipsize 21 digest in
  Alcotest.(check string) "the middle is what goes" "sha256:a3...5f0e1d2c3" cut;
  Alcotest.(check int) "and the result fits" 21 (Width.string_width cut);
  Alcotest.(check string)
    "an odd room gives the head the extra column" "sha256:a37...5f0e1d2c3"
    (Width.ellipsize 22 digest)

(* [git diff --stat] shows a path too long for its column with "..." at the
   front and the cut moved on to the next "/", so a name is never left starting
   in the middle of a directory: [ocaml-console/lib/width.ml] shows as
   [.../lib/width.ml] in a 16 column name and [.../width.ml] in a 12 column one.
   [`Start] is that form. *)
let test_ellipsize_at_start_is_diffstats () =
  let path = "ocaml-console/lib/width.ml" in
  Alcotest.(check string)
    "the front goes, to a slash" ".../lib/width.ml"
    (Width.ellipsize ~at:`Start 16 path);
  Alcotest.(check string)
    "a narrower column drops a directory more" ".../width.ml"
    (Width.ellipsize ~at:`Start 12 path);
  Alcotest.(check string)
    "a cut inside a directory name moves on to its slash" ".../lib/width.ml"
    (Width.ellipsize ~at:`Start 18 "console-it/lib/width.ml");
  Alcotest.(check string)
    "a value with no slash keeps its tail" "...5f0e1d2c3"
    (Width.ellipsize ~at:`Start 12 digest);
  Alcotest.(check string)
    "the default is the middle"
    (Width.ellipsize 21 digest)
    (Width.ellipsize ~at:`Middle 21 digest)

(* A cut closes what it left open, and only that: the reset stands between the
   kept head and the mark, so the mark is not drawn in the value's style, and a
   part that already ends on a reset (SGR 0, ECMA-48 8.3.117) gets no second
   one. *)
let test_ellipsize_closes_style () =
  let check = Alcotest.(check string) in
  check "a head left open is closed before the mark" "\027[1mabc\027[0m...hij"
    (Width.ellipsize 9 "\027[1mabcdefghij");
  check "a head that closed its style is not closed again"
    "\027[1mab\027[0mc...hij"
    (Width.ellipsize 9 "\027[1mab\027[0mcdefghij");
  check "nor is one that ends on its reset" "\027[1mabc\027[0m...hij"
    (Width.ellipsize 9 "\027[1mabc\027[0mdefghij");
  check "a tail left open is closed" "abc...\027[1mhij\027[0m"
    (Width.ellipsize 9 "abcdefg\027[1mhij");
  check "a tail that ends on its reset is not closed again"
    "abc...\027[1mhij\027[0m"
    (Width.ellipsize 9 "abcdefg\027[1mhij\027[0m");
  check "nor is a tail cut at the start" "...\027[1mefghij\027[0m"
    (Width.ellipsize ~at:`Start 9 "abcd\027[1mefghij\027[0m")

(* The same holds of a cut with no mark. *)
let test_truncate_closes_style () =
  List.iter
    (fun (msg, expected, s) ->
      Alcotest.(check string) msg expected (Width.truncate 3 s))
    [
      ("a style left open is closed", "\027[1mabc\027[0m", "\027[1mabcdef");
      ( "a closed style is not closed again",
        "\027[1mab\027[0mc",
        "\027[1mab\027[0mcdef" );
      ( "nor is one that ends on its reset",
        "\027[1mabc\027[0m",
        "\027[1mabc\027[0mdef" );
    ]

let test_ellipsize_short_enough () =
  Alcotest.(check string)
    "a value that fits is untouched" "abc" (Width.ellipsize 3 "abc");
  Alcotest.(check string)
    "and so is a shorter one" "abc" (Width.ellipsize 8 "abc")

let test_ellipsize_no_room () =
  Alcotest.(check string)
    "nothing fits in nothing" "" (Width.ellipsize 0 digest);
  Alcotest.(check string)
    "three columns say only that it was cut" "..." (Width.ellipsize 3 digest);
  Alcotest.(check string) "and fewer, less" ".." (Width.ellipsize 2 digest)

(* A row that does not fit is shortened by words, never
   by characters, and a path, an id or a digest is always printed whole. *)
let waiting = "Target: waiting for op-hello's boot verdict (300s bound)"

let test_shorten_drops_words_from_the_end () =
  Alcotest.(check string)
    "the bracketed group goes as one word"
    "Target: waiting for op-hello's boot verdict" (Width.shorten 50 waiting);
  Alcotest.(check string)
    "and then the words before it, never ending on one that leads into what \
     went"
    "Target: waiting" (Width.shorten 22 waiting);
  Alcotest.(check string)
    "nor on the colon that introduced it" "Target" (Width.shorten 10 waiting);
  Alcotest.(check string)
    "a value that fits is untouched" waiting
    (Width.shorten 200 waiting);
  Alcotest.(check string)
    "a separator left at the end goes with its word" "kept 76 of 96 frames"
    (Width.shorten 22 "kept 76 of 96 frames, dropped 20")

let test_shorten_never_cuts_a_word () =
  Alcotest.(check string)
    "a digest is kept whole or not at all" "" (Width.shorten 21 digest);
  Alcotest.(check string)
    "and so is a path after a word" "pushed"
    (Width.shorten 40 ("pushed " ^ digest))

let test_shorten_closes_a_style () =
  let styled = "\027[1mone two three\027[0m" in
  Alcotest.(check string)
    "the style open at the cut is closed" "\027[1mone two\027[0m"
    (Width.shorten 9 styled)

let test_fold_breaks_at_spaces () =
  Alcotest.(check (list string))
    "each line fits, broken where a space was"
    [ "Target: waiting for"; "op-hello's boot"; "verdict (300s bound)" ]
    (Width.fold 20 waiting);
  Alcotest.(check (list string))
    "a value that fits is one line" [ waiting ] (Width.fold 80 waiting)

let test_fold_keeps_a_word_whole () =
  Alcotest.(check (list string))
    "a digest wider than the line stands whole on its own"
    [ "pushed"; digest; "to the registry" ]
    (Width.fold 20 ("pushed " ^ digest ^ " to the registry"));
  let split = Width.fold ~split:true 20 digest in
  Alcotest.(check string)
    "split, every character is still printed" digest (String.concat "" split);
  List.iter
    (fun line ->
      Alcotest.(check bool)
        "and every line fits" true
        (Width.string_width line <= 20))
    split

let test_fold_reopens_a_style () =
  Alcotest.(check (list string))
    "a style open at a break is closed and opened again"
    [ "\027[1mone\027[0m"; "\027[1mtwo\027[0m" ]
    (Width.fold 4 "\027[1mone two\027[0m")

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

(* A backquoted run is a command the reader copies back whole, so a row never
   ends inside one; one wider than the row takes a row of its own. *)
let test_wrap_keeps_backquoted_run () =
  Alcotest.(check string)
    "run on one row" "use\n`space run a/b.yaml`,\nthen go"
    (Width.wrap 12 "use `space run a/b.yaml`, then go")

(* A word that opens and closes its own run groups nothing after it. *)
let test_wrap_closed_backquote () =
  Alcotest.(check string)
    "closed in one word" "`a`\nb c\n`d`"
    (Width.wrap 4 "`a` b c `d`")

(* A backquote that never closes groups nothing, and every space is a break. *)
let test_wrap_unclosed_backquote () =
  Alcotest.(check string) "unclosed" "a `b c\nd e" (Width.wrap 6 "a `b c d e")

let test_wrap_multiline_input () =
  let result = Width.wrap 20 "line one\nline two\nline three" in
  Alcotest.(check string)
    "normalizes newlines" "line one line two\nline three" result

let test_wrap_empty () =
  let result = Width.wrap 20 "" in
  Alcotest.(check string) "empty string" "" result

let test_wrap_uses_terminal_cells () =
  Alcotest.(check string) "uses terminal cells" "界 a\nb" (Width.wrap 4 "界 a b")

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

(* Width reads escape codes with the scanner {!Console_text.strip} uses, so a
   code strip removes takes no column here either. *)
let test_width_agrees_with_strip () =
  List.iter
    (fun s ->
      Alcotest.(check int)
        (String.escaped s)
        (Width.string_width (Console_text.strip s))
        (Width.string_width s))
    [
      "a\027Pq#0;2\027\\b";
      "a\027]0;title\007b";
      "a\027_apc\027\\b";
      "a\u{009B}31mb";
      "a\027(Bb";
    ]

let suite =
  ( "width",
    [
      Alcotest.test_case "width agrees with strip" `Quick
        test_width_agrees_with_strip;
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
      Alcotest.test_case "ellipsize at the start is diffstat's" `Quick
        test_ellipsize_at_start_is_diffstats;
      Alcotest.test_case "ellipsize closes a style" `Quick
        test_ellipsize_closes_style;
      Alcotest.test_case "truncate closes a style" `Quick
        test_truncate_closes_style;
      Alcotest.test_case "ellipsize leaves a short value" `Quick
        test_ellipsize_short_enough;
      Alcotest.test_case "ellipsize with no room" `Quick test_ellipsize_no_room;
      Alcotest.test_case "shorten drops words from the end" `Quick
        test_shorten_drops_words_from_the_end;
      Alcotest.test_case "shorten never cuts a word" `Quick
        test_shorten_never_cuts_a_word;
      Alcotest.test_case "shorten closes a style" `Quick
        test_shorten_closes_a_style;
      Alcotest.test_case "fold breaks at spaces" `Quick
        test_fold_breaks_at_spaces;
      Alcotest.test_case "fold keeps a word whole" `Quick
        test_fold_keeps_a_word_whole;
      Alcotest.test_case "fold reopens a style" `Quick test_fold_reopens_a_style;
      Alcotest.test_case "wrap basic" `Quick test_wrap_basic;
      Alcotest.test_case "wrap with indent" `Quick test_wrap_with_indent;
      Alcotest.test_case "wrap short" `Quick test_wrap_short;
      Alcotest.test_case "wrap single long word" `Quick
        test_wrap_single_long_word;
      Alcotest.test_case "wrap keeps backquoted run" `Quick
        test_wrap_keeps_backquoted_run;
      Alcotest.test_case "wrap closed backquote" `Quick
        test_wrap_closed_backquote;
      Alcotest.test_case "wrap unclosed backquote" `Quick
        test_wrap_unclosed_backquote;
      Alcotest.test_case "wrap multiline input" `Quick test_wrap_multiline_input;
      Alcotest.test_case "wrap empty" `Quick test_wrap_empty;
      Alcotest.test_case "wrap terminal cells" `Quick
        test_wrap_uses_terminal_cells;
    ] )
