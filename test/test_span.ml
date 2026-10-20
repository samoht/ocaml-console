(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let strings = List.map Span.to_string

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
  Alcotest.(check int)
    "a spacing mark adds a cell to its cluster" 2 (Span.width joined);
  Alcotest.(check int) "style changes do not add cells" 2 (Span.width split);
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
  (* A VS16 stays with its letter, at whatever width they take. *)
  let cut = Span.truncate 3 (Span.text "a\u{fe0f}bcdef") in
  Alcotest.(check bool)
    "a variation selector stays with its letter" true
    (Span.width cut <= 3
    && List.mem (Span.to_string cut) [ "a\u{fe0f}…"; "a\u{fe0f}b…" ]);
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
  (* RFC 3986, section 2.1: a byte that may not appear in a URI is
     percent-encoded, as two upper-case hexadecimal digits. *)
  let target uri =
    let ansi = Span.to_string ~styled:true (Span.link ~uri "x") in
    match String.index_opt ansi '\027' with
    | Some 0 when String.starts_with ~prefix:"\027]8;;" ansi ->
        let rest = String.sub ansi 5 (String.length ansi - 5) in
        String.sub rest 0 (String.index rest '\027')
    | _ -> ""
  in
  let check msg uri expected =
    Alcotest.(check string) msg expected (target uri)
  in
  check "an escape" "https://example.com\027bad" "https://example.com%1Bbad";
  check "a space" "https://example.com/a b" "https://example.com/a%20b";
  check "a BEL" "https://example.com/\007" "https://example.com/%07";
  check "a DEL" "https://example.com/\127" "https://example.com/%7F";
  check "UTF-8" "https://example.com/caf\u{e9}" "https://example.com/caf%C3%A9";
  check "characters RFC 3986 leaves out" "https://example.com/<a|b>{\"^`}\\"
    "https://example.com/%3Ca%7Cb%3E%7B%22%5E%60%7D%5C";
  check "reserved, unreserved and escapes kept"
    "https://u:p@example.com:8/a-b._~c/!$&'()*+,;=?q=[1]#f%20"
    "https://u:p@example.com:8/a-b._~c/!$&'()*+,;=?q=[1]#f%20";
  Alcotest.(check string)
    "an empty URI links nowhere" "x"
    (Span.to_string ~styled:true (Span.link ~uri:"" "x"));
  (* A form feed ends a line, as a line feed does: such a label has no text
     to click, and the link is written over the lines that have some. *)
  Alcotest.(check string)
    "a label of line ends links nothing" "\n\n"
    (Span.to_string ~styled:true (Span.link ~uri:"https://x" "\012\n"));
  Alcotest.(check string)
    "only the lines with text" "\n\027]8;;https://x\027\\a\027]8;;\027\\"
    (Span.to_string ~styled:true (Span.link ~uri:"https://x" "\012a"))

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
    "16 colours by default" "\027[91mx\027[0m" (render None);
  Alcotest.(check string)
    "true colour" "\027[38;2;255;0;0mx\027[0m"
    (render (Some `True_color));
  Alcotest.(check string)
    "256 colours" "\027[38;5;196mx\027[0m" (render (Some `Ansi_256));
  Alcotest.(check string)
    "16 colours" "\027[91mx\027[0m" (render (Some `Ansi_16))

let test_multiline_resets () =
  let output = Span.to_string ~styled:true (Span.styled Style.bold "a\nb") in
  Alcotest.(check string)
    "close before LF and reopen on the next line"
    "\027[1ma\027[0m\n\027[1mb\027[0m" output

let test_zero_width_blank_lines () =
  Alcotest.(check string)
    "zero width returns empty even for blank lines" ""
    (Span.to_string (Span.truncate 0 (Span.text "\n")))

(* [span] printed on a formatter in [theme] at [depth], in colour iff
   [depth] is given. *)
let printed ?(theme = Theme.default) ?depth span =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Option.iter
    (fun depth ->
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf depth)
    depth;
  Span.pp ppf span;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let fg c s = Span.styled (Style.fg c) s

(* Each grapheme cluster of a line takes the blend of the stops at its
   place, the first cluster the first stop and the last the last one. *)
let test_gradient () =
  let red = Color.rgb 255 0 0 and blue = Color.rgb 0 0 255 in
  let check what want got =
    Alcotest.(check string)
      what
      (printed ~depth:`True_color want)
      (printed ~depth:`True_color got)
  in
  check "three stops of two colours"
    Span.(fg red "a" ++ fg (Color.blend red blue 0.5) "b" ++ fg blue "c")
    (Span.gradient ~colors:[ red; blue ] "abc");
  let green = Color.rgb 0 255 0 in
  check "five clusters, three stops"
    Span.(
      concat
        [
          fg red "a";
          fg (Color.blend red green 0.5) "b";
          fg green "c";
          fg (Color.blend green blue 0.5) "d";
          fg blue "e";
        ])
    (Span.gradient ~colors:[ red; green; blue ] "abcde");
  check "a cluster is one colour"
    Span.(fg red "e\u{301}" ++ fg blue "\u{754c}")
    (Span.gradient ~colors:[ red; blue ] "e\u{301}\u{754c}");
  check "one colour" (fg red "ab") (Span.gradient ~colors:[ red ] "ab");
  check "each line its own gradient"
    Span.(fg red "a" ++ fg blue "b" ++ text "\n" ++ fg red "c" ++ fg blue "d")
    (Span.gradient ~colors:[ red; blue ] "ab\ncd");
  Alcotest.(check int)
    "as wide as its text" 5
    (Span.width (Span.gradient ~colors:[ red; blue ] "x\u{754c}e\u{301}y"));
  Alcotest.(check string)
    "plain without colour" "x\u{754c}y"
    (printed (Span.gradient ~colors:[ red; blue ] "x\u{754c}y"));
  Alcotest.(check string)
    "sanitized" "a b"
    (Span.to_string (Span.gradient "a\027b"));
  Alcotest.(check int) "empty" 0 (Span.width (Span.gradient ""))

(* Without colours, a gradient takes the palette of the theme it is printed
   in, at the formatter's depth. *)
let test_gradient_theme () =
  let span = Span.gradient "abcdef" in
  let palette = Theme.palette Theme.rainbow in
  let want =
    Span.concat
      (List.mapi (fun i c -> fg c (String.make 1 "abcdef".[i])) palette)
  in
  List.iter
    (fun depth ->
      Alcotest.(check string)
        "the rainbow's palette"
        (printed ~theme:Theme.rainbow ~depth want)
        (printed ~theme:Theme.rainbow ~depth span))
    [ `Ansi_16; `Ansi_256; `True_color ];
  Alcotest.(check string)
    "a theme of one colour"
    (printed ~depth:`True_color (fg (Theme.accent Theme.default) "abcdef"))
    (printed ~depth:`True_color span);
  Alcotest.(check string)
    "plain without colour" "abcdef"
    (printed ~theme:Theme.rainbow span)

(* A role takes its style from the theme of the formatter it is printed on,
   when it is printed. *)
let test_role () =
  let check what theme want got =
    Alcotest.(check string)
      what
      (printed ~theme ~depth:`True_color want)
      (printed ~theme ~depth:`True_color got)
  in
  List.iter
    (fun theme ->
      let name = Theme.name theme in
      check (name ^ " accent") theme
        (fg (Theme.accent theme) "x")
        (Span.role `Accent "x");
      check (name ^ " ok") theme (fg (Theme.ok theme) "x") (Span.role `Ok "x");
      check (name ^ " warn") theme
        (fg (Theme.warn theme) "x")
        (Span.role `Warn "x");
      check (name ^ " fail") theme
        (fg (Theme.fail theme) "x")
        (Span.role `Fail "x");
      check (name ^ " muted") theme
        (fg (Theme.muted theme) "x")
        (Span.role `Muted "x");
      check (name ^ " header") theme
        (Span.styled (Theme.header theme) "x")
        (Span.role `Header "x"))
    [ Theme.default; Theme.nord; Theme.dos ];
  let nord = Theme.ok Theme.nord in
  check "under a style" Theme.nord
    (Span.styled Style.(bold + fg nord) "passed")
    (Span.with_style Style.bold (Span.role `Ok "passed"));
  check "each line" Theme.nord (fg nord "a\nb") (Span.role `Ok "a\nb");
  check "cut" Theme.nord
    (Span.truncate 3 (fg nord "abcdef"))
    (Span.truncate 3 (Span.role `Ok "abcdef"));
  Alcotest.(check string)
    "plain without colour" "passed"
    (printed ~theme:Theme.nord (Span.role `Ok "passed"));
  Alcotest.(check string)
    "to_string in the default theme"
    (Span.to_string ~styled:true (fg (Theme.ok Theme.default) "x"))
    (Span.to_string ~styled:true (Span.role `Ok "x"));
  Alcotest.(check int)
    "as wide as its text" 3
    (Span.width (Span.role `Ok "a\u{754c}"));
  Alcotest.(check string)
    "sanitized" "a b"
    (Span.to_string (Span.role `Fail "a\027b"))

let suite =
  ( "span",
    [
      Alcotest.test_case "zero-width truncation of blank lines is empty" `Quick
        test_zero_width_blank_lines;
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
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "runs" `Quick test_runs;
      Alcotest.test_case "depth" `Quick test_depth;
      Alcotest.test_case "gradient" `Quick test_gradient;
      Alcotest.test_case "role" `Quick test_role;
      Alcotest.test_case "gradient in the theme's palette" `Quick
        test_gradient_theme;
    ] )
