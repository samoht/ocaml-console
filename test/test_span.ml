(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let strings = List.map Span.to_string
let contains affix s = Re.execp (Re.compile (Re.str affix)) s

let test_text () =
  Alcotest.(check int) "span width" 5 (Span.width (Span.text "hello"));
  Alcotest.(check int)
    "concat width" 11
    (Span.width Span.(text "hello" ++ text " world"));
  Alcotest.(check string)
    "plain string" "hello"
    (Span.to_string (Span.styled Style.bold "hello"))

(* A span holds only text that prints: a control is a space, malformed UTF-8
   the replacement character, and a line feed starts a line. *)
let test_sanitized () =
  let span = Span.styled Style.bold "a\tb\nc\027[2Jd\255" in
  Alcotest.(check string)
    "controls are spaces" "a b\nc [2Jd\u{FFFD}" (Span.to_string span);
  Alcotest.(check bool)
    "no escape from the caller" false
    (contains "[2J\027" (Span.to_string ~styled:true span));
  Alcotest.(check int) "widest line" 7 (Span.width span)

let test_width_boundaries () =
  let suffix = "\xea\xa8\xb4" in
  let joined = Span.text ("x" ^ suffix) in
  let split = Span.(text "x" ++ styled Style.bold suffix) in
  Alcotest.(check int) "joined cluster occupies one cell" 1 (Span.width joined);
  Alcotest.(check int) "style changes do not add cells" 1 (Span.width split);
  Alcotest.(check int) "wide" 4 (Span.width (Span.text "界界"))

let test_truncate () =
  Alcotest.(check string)
    "fits" "hello"
    (Span.to_string (Span.truncate 5 (Span.text "hello")));
  Alcotest.(check string)
    "ellipsis" "hel…"
    (Span.to_string (Span.truncate 4 (Span.text "hello")));
  Alcotest.(check string)
    "one cell" "…"
    (Span.to_string (Span.truncate 1 (Span.text "hello")));
  Alcotest.(check string)
    "nothing" ""
    (Span.to_string (Span.truncate 0 (Span.text "hello")));
  Alcotest.(check string)
    "a wide character is not cut" "界…"
    (Span.to_string (Span.truncate 4 (Span.text "界界界")));
  Alcotest.(check string)
    "every line" "ab…\nc"
    (Span.to_string (Span.truncate 3 (Span.text "abcd\nc")));
  Alcotest.(check string)
    "the style is closed" "\027[1mhe…\027[0m"
    (Span.to_string ~styled:true
       (Span.truncate 3 (Span.styled Style.bold "hello")))

let test_truncate_preserves_link () =
  let span = Span.link ~uri:"https://example.com" "documentation" in
  let shown = Span.truncate 5 span in
  Alcotest.(check string) "visible prefix" "docu…" (Span.to_string shown);
  Alcotest.(check bool)
    "link metadata remains" true
    (contains "\027]8;;https://example.com" (Span.to_string ~styled:true shown))

let test_wrap () =
  let wrap w s = strings (Span.wrap w (Span.text s)) in
  Alcotest.(check (list string))
    "words" [ "hello"; "world" ] (wrap 6 "hello world");
  Alcotest.(check (list string))
    "punctuation stays with its word" [ "abc,"; "def" ] (wrap 4 "abc, def");
  Alcotest.(check (list string))
    "spaces inside a line are kept" [ "a   b"; "c" ] (wrap 5 "a   b c");
  Alcotest.(check (list string))
    "leading spaces are kept" [ "  ab"; "cd" ] (wrap 4 "  ab cd");
  Alcotest.(check (list string))
    "a long word goes on" [ "abc"; "def"; "g" ] (wrap 3 "abcdefg");
  Alcotest.(check (list string))
    "after a slash" [ "src/"; "lib/"; "a.ml" ] (wrap 5 "src/lib/a.ml");
  Alcotest.(check (list string)) "empty" [ "" ] (wrap 3 "");
  Alcotest.(check (list string)) "lines" [ "a"; "b" ] (wrap 3 "a\nb");
  Alcotest.check_raises "zero width"
    (Invalid_argument "Console.Span.wrap: non-positive width") (fun () ->
      ignore (Span.wrap 0 (Span.text "a")))

let test_wrap_malformed () =
  Alcotest.(check (list string))
    "each byte sequence remains visible" [ "\u{FFFD}"; "a" ]
    (strings (Span.wrap 1 (Span.text "\255a")))

let test_wrap_oversized () =
  (* The two-cell glyph cannot fit, but the following ASCII character can. *)
  Alcotest.(check (list string))
    "retain the suffix" [ "a" ]
    (strings (Span.wrap 1 (Span.text "界a")))

let test_wrap_preserves_styles () =
  let span =
    Span.(
      styled Style.(fg Color.red) "hello "
      ++ styled Style.(fg Color.blue) "world")
  in
  let lines = Span.wrap 6 span in
  Alcotest.(check (list string))
    "visible lines" [ "hello"; "world" ] (strings lines);
  let ansi = List.map (Span.to_string ~styled:true) lines in
  Alcotest.(check bool)
    "red first line" true
    (contains "\027[31m" (List.nth ansi 0));
  Alcotest.(check bool)
    "blue second line" true
    (contains "\027[34m" (List.nth ansi 1))

(* Every line after the first starts under the text after the first word, and
   still fits; a hang of more than half the width is clamped to half. *)
let test_wrap_hang () =
  Alcotest.(check (list string))
    "hanging"
    [ "abcd efgh"; "     ijkl"; "     mnop" ]
    (strings (Span.wrap_hanging 10 (Span.text "abcd efgh ijkl mnop")));
  Alcotest.(check (list string))
    "clamped"
    [ "abcdef"; "   gh"; "   ij" ]
    (strings (Span.wrap_hanging 7 (Span.text "abcdef gh ij")))

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
  Alcotest.check_raises "unsafe URI"
    (Invalid_argument "Console.Span.link: unsafe URI character") (fun () ->
      ignore (Span.link ~uri:"https://example.com\027bad" "bad"));
  Alcotest.check_raises "space in URI"
    (Invalid_argument "Console.Span.link: unsafe URI character") (fun () ->
      ignore (Span.link ~uri:"https://example.com/a b" "bad"));
  Alcotest.check_raises "empty URI"
    (Invalid_argument "Console.Span.link: empty URI") (fun () ->
      ignore (Span.link ~uri:"" "bad"))

(* One run of a style is written under one escape code. *)
let test_runs () =
  Alcotest.(check string)
    "merged" "\027[1mab\027[0m"
    (Span.to_string ~styled:true
       Span.(styled Style.bold "a" ++ styled Style.bold "b"))

(* The colours of a formatter's depth. *)
let test_depth () =
  let render depth =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Option.iter (Color.set_depth ppf) depth;
    Span.pp ppf (Span.styled (Style.fg (Color.rgb 255 0 0)) "x");
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check string)
    "true colour by default" "\027[38;2;255;0;0mx\027[0m" (render None);
  Alcotest.(check string)
    "256 colours" "\027[38;5;196mx\027[0m" (render (Some `Ansi_256));
  Alcotest.(check string)
    "16 colours" "\027[91mx\027[0m" (render (Some `Ansi_16))

let test_multiline_resets () =
  let output = Span.to_string ~styled:true (Span.styled Style.bold "a\nb") in
  Alcotest.(check string)
    "close before LF and reopen on the next line"
    "\027[1ma\027[0m\n\027[1mb\027[0m" output

let suite =
  ( "span",
    [
      Alcotest.test_case "multiline styles reset before LF" `Quick
        test_multiline_resets;
      Alcotest.test_case "text" `Quick test_text;
      Alcotest.test_case "sanitized" `Quick test_sanitized;
      Alcotest.test_case "width is independent of styling boundaries" `Quick
        test_width_boundaries;
      Alcotest.test_case "truncate" `Quick test_truncate;
      Alcotest.test_case "truncate preserves link" `Quick
        test_truncate_preserves_link;
      Alcotest.test_case "wrap" `Quick test_wrap;
      Alcotest.test_case "wrapping replaces malformed UTF-8" `Quick
        test_wrap_malformed;
      Alcotest.test_case "wrapping retains text after an oversized glyph" `Quick
        test_wrap_oversized;
      Alcotest.test_case "wrap preserves styles" `Quick
        test_wrap_preserves_styles;
      Alcotest.test_case "wrap hang" `Quick test_wrap_hang;
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "runs" `Quick test_runs;
      Alcotest.test_case "depth" `Quick test_depth;
    ] )
