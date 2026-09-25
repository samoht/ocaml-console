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
  let plain = Span.to_string span in
  Alcotest.(check string) "plain string" "hello" plain

let test_sanitize () =
  let span = Span.styled Style.bold "a\tb\nc\027d" in
  Alcotest.(check string)
    "keeps newlines" "a b\nc d"
    (Span.to_string (Span.sanitize span));
  Alcotest.(check string)
    "single line" "a b c d"
    (Span.to_string (Span.sanitize ~keep_newlines:false span))

let test_wrap_preserves_styles () =
  let span =
    Span.(
      styled Style.(fg Color.red) "hello "
      ++ styled Style.(fg Color.blue) "world")
  in
  let lines = Span.wrap 6 span in
  Alcotest.(check (list string))
    "visible lines" [ "hello"; "world" ]
    (List.map Span.to_string lines);
  let ansi = List.map Span.to_ansi_string lines in
  Alcotest.(check bool)
    "red first line" true
    (Re.execp (Re.compile (Re.str "\027[31m")) (List.nth ansi 0));
  Alcotest.(check bool)
    "blue second line" true
    (Re.execp (Re.compile (Re.str "\027[34m")) (List.nth ansi 1))

let test_truncate_preserves_link () =
  let span = Span.link ~uri:"https://example.com" "documentation" in
  let shown = Span.truncate 4 span in
  Alcotest.(check string) "visible prefix" "docu" (Span.to_string shown);
  Alcotest.(check bool)
    "link metadata remains" true
    (Re.execp
       (Re.compile (Re.str "\027]8;;https://example.com"))
       (Span.to_ansi_string shown))

let test_link () =
  let span =
    Span.link ~style:Style.underline ~uri:"https://example.com" "docs"
  in
  Alcotest.(check string) "plain label" "docs" (Span.to_string span);
  let ansi = Span.to_ansi_string span in
  Alcotest.(check bool)
    "OSC 8 open" true
    (String.starts_with ~prefix:"\027]8;;https://example.com\027\\" ansi);
  Alcotest.(check bool)
    "OSC 8 close" true
    (String.ends_with ~suffix:"\027]8;;\027\\" ansi);
  Alcotest.(check int) "OSC 8 has label width" 4 (Width.string_width ansi);
  Alcotest.check_raises "unsafe URI"
    (Invalid_argument "Console.Span.link: unsafe URI character") (fun () ->
      ignore (Span.link ~uri:"https://example.com\027bad" "bad"));
  Alcotest.check_raises "space in URI"
    (Invalid_argument "Console.Span.link: unsafe URI character") (fun () ->
      ignore (Span.link ~uri:"https://example.com/a b" "bad"));
  Alcotest.check_raises "empty URI"
    (Invalid_argument "Console.Span.link: empty URI") (fun () ->
      ignore (Span.link ~uri:"" "bad"))

(* [anim] is the still animation: every frame is [to_string], so a span lays out
   in an animated Layout next to moving widgets. *)
let test_anim () =
  let span = Span.text "hello" in
  let a = Span.anim span in
  Alcotest.(check string)
    "frame matches to_string" (Span.to_string span)
    (Anim.frame a ~elapsed:2.0)

(* Every line after the first starts with [hang] spaces and still fits; a hang
   of more than half the width is clamped to half. *)
let test_wrap_hang () =
  let span = Span.text "abcd efgh ijkl mnop" in
  Alcotest.(check (list string))
    "hanging"
    [ "abcd efgh"; "    ijkl"; "    mnop" ]
    (List.map Span.to_string (Span.wrap ~hang:4 10 span));
  Alcotest.(check (list string))
    "clamped"
    [ "abcd"; "   efgh"; "   ijkl"; "   mnop" ]
    (List.map Span.to_string (Span.wrap ~hang:5 7 span))

(* The text after a label: leading spaces, the first word, the spaces after. *)
let test_hanging () =
  let hanging s = Span.hanging (Span.text s) in
  Alcotest.(check int) "label" 8 (hanging "REFUSED main has no baseline");
  Alcotest.(check int) "indented" 7 (hanging "  1/2  id");
  Alcotest.(check int)
    "styled label" 8
    (Span.hanging Span.(styled Style.bold "REFUSED" ++ text " the reason"));
  Alcotest.(check int) "one word" 0 (hanging "word");
  Alcotest.(check int) "trailing spaces" 0 (hanging "word   ")

let suite =
  ( "span",
    [
      Alcotest.test_case "text" `Quick test_text;
      Alcotest.test_case "concat" `Quick test_concat;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "wrap preserves styles" `Quick
        test_wrap_preserves_styles;
      Alcotest.test_case "wrap hang" `Quick test_wrap_hang;
      Alcotest.test_case "hanging" `Quick test_hanging;
      Alcotest.test_case "truncate preserves link" `Quick
        test_truncate_preserves_link;
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "anim" `Quick test_anim;
    ] )
