module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Width" (module Console.Width : Surface);
  exposed "Guide" (module Console.Guide : Surface);
  exposed "Rule" (module Console.Rule : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface)

(* The library's terminal-geometry contract says that malformed UTF-8 renders as
   a replacement glyph and that terminal controls in content cannot move the
   cursor. Every widget applies that to what it lays out. A caller printing a
   string of its own -- a subprocess's stderr, a peer's name off the network --
   needs the same rule over a plain string, and the only way to reach it was to
   wrap the string in a Span and unwrap the result, so callers write private
   ANSI filters instead and each one draws its own line between a control
   character and a printable one. *)
let test_sanitize () =
  Alcotest.(check string)
    "a tab, a newline and an escape all become spaces" "a b c d"
    (Console.sanitize ~keep_newlines:false "a\tb\nc\027d");
  Alcotest.(check string)
    "a newline is a row boundary, so it is kept by default" "a b\nc d"
    (Console.sanitize "a\tb\nc\027d");
  Alcotest.(check string)
    "a malformed sequence becomes the replacement glyph" "a\u{FFFD}b"
    (Console.sanitize "a\255b");
  Alcotest.(check string)
    "a C1 control, which UTF-8 hides behind two bytes, becomes a space" "a b"
    (Console.sanitize "a\u{009B}b");
  Alcotest.(check string)
    "DEL becomes a space" "a b"
    (Console.sanitize "a\127b");
  Alcotest.(check string)
    "well-formed text, wide glyphs included, is returned unchanged"
    "\u{4E16}\u{754C} ok"
    (Console.sanitize "\u{4E16}\u{754C} ok")

(* A caller that pages a subprocess's output wants the colours it already
   carries and none of the rest of ANSI. ECMA-48 8.3.117 SELECT GRAPHIC
   RENDITION is the only control sequence that cannot move the cursor, erase a
   region or name a resource, so it is the only one a sanitizer may forward and
   still promise what {!Console.sanitize} promises. Recognising it means parsing
   it: CSI, parameter bytes, final byte 'm', and this library accepts a strict
   subset of the parameter bytes ECMA-48 5.4 allows -- the digits and ';' -- so
   the private forms reserved there and the ':' sub-parameter of ITU-T T.416
   are refused rather than forwarded on a guess. *)
let styles s = Console.sanitize_styles s

(* Everything that is not a strictly-parsed SGR is what {!Console.sanitize}
   makes of it, and one function decides that for both. *)
let neutralised what s =
  Alcotest.(check string) what (Console.sanitize s) (styles s)

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
    (Console.sanitize_styles ~keep_newlines:false "\027[31ma\nb\027[0m")

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
   it, which is the half of {!Console.sanitize}'s promise a styled variant can
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

(* no-color.org: a non-empty NO_COLOR turns colour off; an explicit --color
   wins; TERM unset, empty or dumb is no colour; else a terminal is styled. *)
let test_style_renderer () =
  let decide ?renderer ~is_tty pairs =
    match
      Console.style_renderer ?renderer
        ~getenv:(fun name -> List.assoc_opt name pairs)
        ~is_tty ()
    with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let term = ("TERM", "xterm-256color") in
  let check label expected got = Alcotest.(check string) label expected got in
  check "terminal" "ansi" (decide ~is_tty:true [ term ]);
  check "pipe" "none" (decide ~is_tty:false [ term ]);
  check "NO_COLOR" "none" (decide ~is_tty:true [ term; ("NO_COLOR", "1") ]);
  check "empty NO_COLOR" "ansi" (decide ~is_tty:true [ term; ("NO_COLOR", "") ]);
  check "TERM dumb" "none" (decide ~is_tty:true [ ("TERM", "dumb") ]);
  check "TERM empty" "none" (decide ~is_tty:true [ ("TERM", "") ]);
  check "TERM unset" "none" (decide ~is_tty:true []);
  check "--color=always" "ansi"
    (decide ~renderer:`Ansi_tty ~is_tty:false [ ("NO_COLOR", "1") ]);
  check "--color=never" "none" (decide ~renderer:`None ~is_tty:true [ term ])

(* What a terminal shows of a string: ECMA-48 (5th ed.) gives the grammar of
   everything else. Each case is [(what, raw, visible)]; a sequence a terminal
   consumes is gone whole, a malformed UTF-8 sequence is the replacement glyph
   and any other control is a space, as {!Console.sanitize} makes it. *)
let stripped =
  [
    (* Annex B.1, CURSOR RIGHT by one in 7-bit and 8-bit codes, 01/11 05/11
       03/01 04/03 and 09/11 03/01 04/03; 8.3.16 gives CSI as 09/11, which
       UTF-8 writes as U+009B. Leading zeros and the default are B.1's own. *)
    ("CUF, 7-bit", "a\027[1Cb", "ab");
    ("CUF, 8-bit", "a\u{009B}1Cb", "ab");
    ("CUF, leading zero", "a\027[01Cb", "ab");
    ("CUF, default", "a\u{009B}Cb", "ab");
    (* B.1, SCROLL RIGHT by 28: an intermediate byte 02/00 before F. *)
    ("SR, 7-bit", "a\027[28 Ab", "ab");
    ("SR, 8-bit", "a\u{009B}28 Ab", "ab");
    (* B.1, DEFINE AREA QUALIFICATION: F 06/15 is lowercase. *)
    ("DAQ", "a\027[3;4ob", "ab");
    (* 8.3.117 SGR, the sequence most text carries. *)
    ("SGR", "\027[1;31mred\027[0m", "red");
    (* 5.4 b: a private parameter string (B.2, =3) is still parameters. *)
    ("private parameters", "a\027[?25lb", "ab");
    (* 5.6 and 8.3.89: OSC opens a control string that ST (8.3.143, 09/12 or
       ESC 05/12) closes; xterm's OSC 8 hyperlink is one, and xterm also
       closes an OSC at BEL. *)
    ( "OSC 8 hyperlink",
      "\027]8;;https://example.com\027\\link\027]8;;\027\\",
      "link" );
    ("OSC, BEL", "\027]0;title\007text", "text");
    ("OSC, 8-bit", "\u{009D}0;title\u{009C}text", "text");
    (* 5.6: a command string holds 00/08 to 00/13, so a line feed inside an
       OSC is the OSC's. *)
    ("OSC over a line feed", "\027]0;a\nb\007c", "c");
    (* 8.3.27 DCS, in both codes; BEL is outside its command string and
       closes only an OSC. *)
    ("DCS, 7-bit", "a\027P1$r0m\027\\b", "ab");
    ("DCS, 8-bit", "a\u{0090}1$r0m\u{009C}b", "ab");
    ("BEL does not close a DCS", "a\027Pq\007#0\027\\b", "ab");
    (* 8.3.128 SOS, 8.3.94 PM, 8.3.2 APC, in both codes; an SOS character
       string may hold any bit combination but SOS and ST. *)
    ("SOS", "a\027Xs\nos\027\\b", "ab");
    ("SOS, 8-bit", "a\u{0098}sos\u{009C}b", "ab");
    ("PM", "a\027^pm\027\\b", "ab");
    ("PM, 8-bit", "a\u{009E}pm\u{009C}b", "ab");
    ("APC", "a\027_apc\027\\b", "ab");
    ("APC, 8-bit", "a\u{009F}apc\u{009C}b", "ab");
    (* A control string the text ends inside is the terminal's to the end. *)
    ("unterminated OSC", "a\027]0;title", "a");
    ("unterminated CSI", "a\027[31", "a");
    ("lone ESC", "a\027", "a");
    (* An ESC inside a control string ends it and opens what follows. *)
    ("ESC ends a string", "a\027]0;t\027[31mb", "ab");
    (* 8.3.6 CAN: what precedes it is in error and ignored. *)
    ("CAN ends a string", "a\027]0;t\024b", "a b");
    (* 5.3: the other C1 controls, ESC Fe and their 8-bit forms, and the
       ECMA-35 escape sequences ESC Fp, ESC Fs and ESC I F. *)
    ("RI, 7-bit", "a\027Mb", "ab");
    ("NEL, 8-bit", "a\u{0085}b", "ab");
    ("lone ST", "a\u{009C}b", "ab");
    ("ESC Fp", "a\0277b", "ab");
    ("ESC Fs", "a\027cb", "ab");
    ("ESC I F", "a\027(Bb", "ab");
    (* A C0 control inside a control sequence acts and the sequence goes on,
       as the DEC parser xterm follows does. *)
    ("C0 inside CSI", "a\027[3\n1mb", "a\nb");
    (* A character outside 5.4's ranges ends the sequence, and is text. *)
    ("CSI cut short", "a\027[3\u{00E9}b", "a\u{00E9}b");
    (* Malformed UTF-8 is the replacement glyph, a lone 09/11 byte too: in a
       UTF-8 stream it is no CSI. *)
    ("malformed UTF-8", "a\255b", "a\u{FFFD}b");
    ("raw 09/11 byte", "a\15531mb", "a\u{FFFD}31mb");
    (* The rest is {!Console.sanitize}'s. *)
    ("tab and DEL", "a\tb\127c", "a b c");
    ("plain text", "\u{4E16}\u{754C} ok", "\u{4E16}\u{754C} ok");
  ]

let test_strip () =
  List.iter
    (fun (what, raw, visible) ->
      Alcotest.(check string) what visible (Console.strip raw))
    stripped;
  Alcotest.(check string)
    "a line feed becomes a space without keep_newlines" "a b"
    (Console.strip ~keep_newlines:false "a\027[3\n1mb")

(* Whatever the input, no ESC and no C1 control reaches the output, and it is
   UTF-8. *)
let test_strip_leaves_no_control () =
  List.iter
    (fun (what, raw, _) ->
      let s = Console.strip raw in
      Alcotest.(check bool) (what ^ ": UTF-8") true (String.is_valid_utf_8 s);
      let rec scan i =
        if i < String.length s then begin
          let d = String.get_utf_8_uchar s i in
          let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
          if (c < 0x20 && c <> 0x0a) || (c >= 0x7f && c <= 0x9f) then
            Alcotest.failf "%s: U+%04X survives in %S" what c s;
          scan (i + Uchar.utf_decode_length d)
        end
      in
      scan 0)
    stripped

let suite =
  ( "console",
    [
      Alcotest.test_case "style renderer" `Quick test_style_renderer;
      Alcotest.test_case "public surface" `Quick test_public_surface;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "strip" `Quick test_strip;
      Alcotest.test_case "strip leaves no control" `Quick
        test_strip_leaves_no_control;
      Alcotest.test_case "sanitize_styles passes SGR" `Quick
        test_sanitize_styles_passes_sgr;
      Alcotest.test_case "sanitize_styles bounds the parameters" `Quick
        test_sanitize_styles_bounds_the_parameters;
      Alcotest.test_case "sanitize_styles neutralises the rest" `Quick
        test_sanitize_styles_neutralises_the_rest;
      Alcotest.test_case "sanitize_styles closes what it opens" `Quick
        test_sanitize_styles_closes_what_it_opens;
    ] )
