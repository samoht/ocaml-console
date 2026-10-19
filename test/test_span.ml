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
  let ansi = List.map (Span.to_string ~styled:true) lines in
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
       (Span.to_string ~styled:true shown))

let test_link () =
  let span =
    Span.link ~style:Style.underline ~uri:"https://example.com" "docs"
  in
  Alcotest.(check string) "plain label" "docs" (Span.to_string span);
  let ansi = Span.to_string ~styled:true span in
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

(* Every line after the first starts under the text after the first word, and
   still fits; a hang of more than half the width is clamped to half. *)
let test_wrap_hang () =
  Alcotest.(check (list string))
    "hanging"
    [ "abcd efgh"; "     ijkl"; "     mnop" ]
    (List.map Span.to_string
       (Span.wrap_hanging 10 (Span.text "abcd efgh ijkl mnop")));
  Alcotest.(check (list string))
    "clamped"
    [ "abcdef"; "   gh"; "   ij" ]
    (List.map Span.to_string (Span.wrap_hanging 7 (Span.text "abcdef gh ij")))

let test_wrap_malformed () =
  Alcotest.(check (list string))
    "each byte sequence remains visible" [ "\u{FFFD}"; "a" ]
    (List.map Span.to_string (Span.wrap 1 (Span.text "\255a")))

let test_wrap_oversized () =
  (* The two-cell glyph cannot fit, but the following ASCII character can. *)
  Alcotest.(check (list string))
    "retain the suffix" [ "a" ]
    (List.map Span.to_string (Span.wrap 1 (Span.text "界a")))

let test_width_boundaries () =
  let suffix = "\xea\xa8\xb4" in
  let joined = Span.text ("x" ^ suffix) in
  let split = Span.(text "x" ++ styled Style.bold suffix) in
  Alcotest.(check int) "joined cluster occupies one cell" 1 (Span.width joined);
  Alcotest.(check int) "style changes do not add cells" 1 (Span.width split)

let suite =
  ( "span",
    [
      Alcotest.test_case "width is independent of styling boundaries" `Quick
        test_width_boundaries;
      Alcotest.test_case "wrapping retains text after an oversized glyph" `Quick
        test_wrap_oversized;
      Alcotest.test_case "wrapping replaces malformed UTF-8" `Quick
        test_wrap_malformed;
      Alcotest.test_case "text" `Quick test_text;
      Alcotest.test_case "concat" `Quick test_concat;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "wrap preserves styles" `Quick
        test_wrap_preserves_styles;
      Alcotest.test_case "wrap hang" `Quick test_wrap_hang;
      Alcotest.test_case "truncate preserves link" `Quick
        test_truncate_preserves_link;
      Alcotest.test_case "link" `Quick test_link;
    ] )
