(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Guide" (module Console.Guide : Surface);
  exposed "Rule" (module Console.Rule : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface)

(* The library's terminal-geometry contract says that malformed UTF-8 renders as
   a replacement glyph and that terminal controls in content cannot move the
   cursor. Every widget applies that to what it lays out. A caller printing a
   string of its own -- a subprocess's stderr, a peer's name off the network --
   needs the same rule over a plain string, which [sanitize] is. *)
let test_sanitize () =
  Alcotest.(check string)
    "a tab, a newline and an escape all become spaces" "a b c d"
    (Console_text.sanitize ~keep_newlines:false "a\tb\nc\027d");
  Alcotest.(check string)
    "a newline is a row boundary, so it is kept by default" "a b\nc d"
    (Console_text.sanitize "a\tb\nc\027d");
  Alcotest.(check string)
    "a malformed sequence becomes the replacement glyph" "a\u{FFFD}b"
    (Console_text.sanitize "a\255b");
  Alcotest.(check string)
    "a C1 control, which UTF-8 hides behind two bytes, becomes a space" "a b"
    (Console_text.sanitize "a\u{009B}b");
  Alcotest.(check string)
    "DEL becomes a space" "a b"
    (Console_text.sanitize "a\127b");
  Alcotest.(check string)
    "well-formed text, wide glyphs included, is returned unchanged"
    "\u{4E16}\u{754C} ok"
    (Console_text.sanitize "\u{4E16}\u{754C} ok")

(* A caller that pages a subprocess's output wants the colours it already
   carries and none of the rest of ANSI. ECMA-48 8.3.117 SELECT GRAPHIC
   RENDITION is the only control sequence that cannot move the cursor, erase a
   region or name a resource, so it is the only one a sanitizer may forward and
   still promise what {!Console_text.sanitize} promises. Recognising it means parsing
   it: CSI, parameter bytes, final byte 'm', and this library accepts a strict
   subset of the parameter bytes ECMA-48 5.4 allows -- the digits and ';' -- so
   the private forms reserved there and the ':' sub-parameter of ITU-T T.416
   are refused rather than forwarded on a guess. *)
let styles s = Console_text.sanitize_styles s

(* Everything that is not a strictly-parsed SGR is removed, as
   {!Console_text.strip} removes it, and one scanner decides that for both. *)
let neutralised what s =
  Alcotest.(check string) what (Console_text.strip s) (styles s)

let test_sanitize_styles_passes_sgr () =
  Alcotest.(check string)
    "a colour and its reset survive byte for byte" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "so does a 24-bit colour, which is eleven numeric parameters"
    "\027[38;2;255;0;0mred\027[0m"
    (styles "\027[38;2;255;0;0mred\027[0m");
  Alcotest.(check string)
    "an empty parameter list is SGR 0 and survives" "\027[mplain"
    (styles "\027[mplain");
  Alcotest.(check string)
    "a control inside styled text still becomes a space" "\027[31ma b\027[0m"
    (styles "\027[31ma\tb\027[0m");
  Alcotest.(check string)
    "and a newline still goes when it is not wanted" "\027[31ma b\027[0m"
    (Console_text.sanitize_styles ~keep_newlines:false "\027[31ma\nb\027[0m")

(* The parameter run is bounded, because an unbounded scan over untrusted bytes
   is a scan whoever wrote the bytes sizes. *)
let test_sanitize_styles_bounds_the_parameters () =
  let params n = String.make n '1' in
  Alcotest.(check string)
    "sixty-four parameter bytes are within the bound"
    ("\027[" ^ params 64 ^ "mx\027[0m")
    (styles ("\027[" ^ params 64 ^ "mx\027[0m"));
  neutralised "sixty-five are not, and neutralise" ("\027[" ^ params 65 ^ "mx");
  neutralised "nor does a sequence the string ends in the middle of"
    "tail\027[31"

let test_sanitize_styles_neutralises_the_rest () =
  neutralised "erase in display moves nothing through" "\027[2Jgone";
  neutralised "nor does cursor position" "\027[10;20Hgone";
  neutralised "nor a private parameter byte, whatever its final byte"
    "\027[?25lgone";
  neutralised "nor a colon sub-parameter, which is not the accepted grammar"
    "\027[38:2:255:0:0mred";
  neutralised "an OSC 8 hyperlink is a spoofing surface, not styling"
    "\027]8;;https://example.invalid/\027\\click\027]8;;\027\\";
  neutralised "a device control string carries whatever it likes"
    "\027Pq#0;2;0;0;0\027\\";
  neutralised "an escape that is not a control sequence at all" "\027(Bgone";
  (* C1 CSI is one code point, not ESC '[', and the ruling neutralises C1. *)
  neutralised "the C1 introducer does not open an SGR" "\u{009B}31mred"

(* The sanitized text cannot change the styling of what a caller prints after
   it, which is the half of {!Console_text.sanitize}'s promise a styled variant can
   still keep. SGR parameters apply left to right and 0 turns every attribute
   off, so a last parameter of 0 -- or an empty one, which SGR reads as 0 --
   already leaves the terminal in its default state and nothing is appended. *)
let test_sanitize_styles_closes_what_it_opens () =
  Alcotest.(check string)
    "an unterminated colour is closed" "\027[31mred\027[0m"
    (styles "\027[31mred");
  Alcotest.(check string)
    "so is one whose last parameter sets an attribute" "\027[0;31mred\027[0m"
    (styles "\027[0;31mred");
  Alcotest.(check string)
    "a trailing reset is not doubled" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "nor is a reset that arrives as the last parameter" "\027[31;0mred"
    (styles "\027[31;0mred");
  let s = "\027[31mred\027[2Jgone\tand\u{009B}31m" in
  Alcotest.(check string)
    "and sanitizing twice changes nothing the first pass did not" (styles s)
    (styles (styles s))

(* {1 Side by side} *)

open Console

let printed ?(styled = false) ?(margin = 80) pp =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color
  end;
  Format.pp_set_margin ppf margin;
  pp ppf ();
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let lines s = String.split_on_char '\n' s
let text s ppf () = Fmt.string ppf s

(* [hcat] puts blocks side by side, [gap] spaces apart, each padded to its
   widest line and a shorter one with blank lines, a line ending at its last
   block that has text there. *)
let test_hcat () =
  Alcotest.(check string)
    "side by side" "a    1\nbbb  2\n     3"
    (printed (hcat [ text "a\nbbb"; text "1\n2\n3" ]));
  Alcotest.(check string)
    "no trailing space" "aa 1\nb\nc"
    (printed (hcat ~gap:1 [ text "aa\nb\nc"; text "1" ]));
  Alcotest.(check string)
    "a blank block line" "a  1\n   2"
    (printed (hcat [ text "a\n"; text "1\n2" ]));
  Alcotest.(check string) "one block" "a\nb" (printed (hcat [ text "a\nb" ]));
  Alcotest.(check string) "none" "" (printed (hcat []));
  let panel = Panel.v (Span.text "p") and tree = Tree.v (Span.text "t") [] in
  let pp = hcat [ Fmt.const Panel.pp panel; Fmt.const Tree.pp tree ] in
  let panel_lines = lines (Panel.to_string panel) in
  Alcotest.(check (list string))
    "widgets"
    [
      List.nth panel_lines 0 ^ "  t";
      List.nth panel_lines 1;
      List.nth panel_lines 2;
    ]
    (lines (printed pp));
  Display_harness.check_invalid "Console.hcat: negative gap" (fun () ->
      ignore (hcat ~gap:(-1) [] : unit Fmt.t))

(* Each block is printed at its share of the margin, the gaps taken out
   first, and keeps its colours. *)
let test_hcat_share () =
  let rule = Fmt.const Rule.pp (Rule.v ()) in
  Alcotest.(check (list int))
    "two rules in 21 columns" [ 21 ]
    (List.map Width.string_width
       (lines (printed ~margin:21 (hcat ~gap:3 [ rule; rule ]))));
  let red = Span.styled (Style.fg Color.red) "red"
  and blue = Span.styled (Style.fg Color.blue) "blue" in
  Alcotest.(check string)
    "colours"
    (Span.to_string ~styled:true red ^ "  " ^ Span.to_string ~styled:true blue)
    (printed ~styled:true
       (hcat [ Fmt.const Span.pp red; Fmt.const Span.pp blue ]))

(* Composing widgets must retain their margin guarantee, even when the gaps
   alone consume all the available columns. *)
let test_hcat_narrow () =
  let rule = Fmt.const Rule.pp (Rule.v ()) in
  let out = printed ~margin:2 (hcat [ rule; rule ]) in
  List.iter
    (fun line ->
      Alcotest.(check bool)
        "two rules fit two columns" true
        (Span.width (Span.text line) <= 2))
    (lines out)

(* A zero-cell block can still contain text: here its accent belongs to
   the preceding block when there is no gap. *)
let test_hcat_combining_suffix () =
  let block s = Fmt.const Span.pp (Span.text s) in
  Alcotest.(check string)
    "the accent survives composition" "e\u{0301}"
    (printed (hcat ~gap:0 [ block "e"; block "\u{0301}" ]))

let suite =
  ( "console",
    [
      Alcotest.test_case "side by side keeps a combining suffix" `Quick
        test_hcat_combining_suffix;
      Alcotest.test_case "side by side fits a two-column margin" `Quick
        test_hcat_narrow;
      Alcotest.test_case "public surface" `Quick test_public_surface;
      Alcotest.test_case "side by side" `Quick test_hcat;
      Alcotest.test_case "side by side, shares of the margin" `Quick
        test_hcat_share;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "sanitize_styles passes SGR" `Quick
        test_sanitize_styles_passes_sgr;
      Alcotest.test_case "sanitize_styles bounds the parameters" `Quick
        test_sanitize_styles_bounds_the_parameters;
      Alcotest.test_case "sanitize_styles neutralises the rest" `Quick
        test_sanitize_styles_neutralises_the_rest;
      Alcotest.test_case "sanitize_styles closes what it opens" `Quick
        test_sanitize_styles_closes_what_it_opens;
    ] )
