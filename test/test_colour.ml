(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Colour = Console__.Colour

let env pairs name = List.assoc_opt name pairs

(* An environment in which reading a variable the rule does not name fails. *)
let only names pairs name =
  if List.mem name names then env pairs name
  else Alcotest.failf "reads %s, which it does not document" name

let test_dumb () =
  let check label expected pairs =
    Alcotest.(check bool) label expected (Colour.dumb ~getenv:(env pairs))
  in
  check "unset" true [];
  check "empty" true [ ("TERM", "") ];
  check "dumb" true [ ("TERM", "dumb") ];
  check "xterm" false [ ("TERM", "xterm") ];
  check "case matters" false [ ("TERM", "DUMB") ];
  check "a prefix is not dumb" false [ ("TERM", "dumb-emacs") ];
  check "a blank is not empty" false [ ("TERM", " ") ];
  check "other variables are not read" true [ ("COLORTERM", "truecolor") ]

let renderer =
  let pp ppf r =
    Fmt.string ppf (match r with `Ansi_tty -> "`Ansi_tty" | `None -> "`None")
  in
  Alcotest.testable pp ( = )

let test_renderer () =
  let term = ("TERM", "xterm") in
  let check label expected ~is_tty pairs =
    Alcotest.check renderer label expected
      (Colour.renderer ~getenv:(env pairs) ~is_tty)
  in
  check "terminal" `Ansi_tty ~is_tty:true [ term ];
  check "not a terminal" `None ~is_tty:false [ term ];
  check "NO_COLOR" `None ~is_tty:true [ term; ("NO_COLOR", "1") ];
  check "NO_COLOR off a terminal" `None ~is_tty:false
    [ term; ("NO_COLOR", "1") ];
  check "NO_COLOR empty" `Ansi_tty ~is_tty:true [ term; ("NO_COLOR", "") ];
  check "any NO_COLOR, even 0" `None ~is_tty:true [ term; ("NO_COLOR", "0") ];
  check "a blank NO_COLOR" `None ~is_tty:true [ term; ("NO_COLOR", " ") ];
  check "TERM unset" `None ~is_tty:true [];
  check "TERM empty" `None ~is_tty:true [ ("TERM", "") ];
  check "TERM dumb" `None ~is_tty:true [ ("TERM", "dumb") ];
  check "empty NO_COLOR does not save a dumb TERM" `None ~is_tty:true
    [ ("TERM", "dumb"); ("NO_COLOR", "") ]

let depth =
  let pp ppf d =
    Fmt.string ppf
      (match d with
      | `True_color -> "`True_color"
      | `Ansi_256 -> "`Ansi_256"
      | `Ansi_16 -> "`Ansi_16")
  in
  Alcotest.testable pp ( = )

let test_depth () =
  let check label expected pairs =
    Alcotest.check depth label expected (Colour.depth ~getenv:(env pairs))
  in
  check "nothing" `Ansi_16 [];
  check "COLORTERM truecolor" `True_color [ ("COLORTERM", "truecolor") ];
  check "COLORTERM 24bit" `True_color [ ("COLORTERM", "24bit") ];
  check "COLORTERM wins over TERM" `True_color
    [ ("COLORTERM", "truecolor"); ("TERM", "xterm-256color") ];
  check "COLORTERM wins over a dumb TERM" `True_color
    [ ("COLORTERM", "24bit"); ("TERM", "dumb") ];
  check "COLORTERM is an exact name" `Ansi_16
    [ ("COLORTERM", "TRUECOLOR"); ("TERM", "xterm") ];
  check "another COLORTERM falls to TERM" `Ansi_256
    [ ("COLORTERM", "yes"); ("TERM", "xterm-256color") ];
  check "empty COLORTERM falls to TERM" `Ansi_256
    [ ("COLORTERM", ""); ("TERM", "xterm-256color") ];
  check "-direct at the end" `True_color [ ("TERM", "xterm-direct") ];
  check "-direct alone" `True_color [ ("TERM", "-direct") ];
  check "-direct wins over 256color" `True_color
    [ ("TERM", "xterm-256color-direct") ];
  check "-direct not at the end" `Ansi_16 [ ("TERM", "xterm-direct-x") ];
  check "direct without its dash" `Ansi_16 [ ("TERM", "direct") ];
  check "256color at the end" `Ansi_256 [ ("TERM", "screen-256color") ];
  check "256color at the start" `Ansi_256 [ ("TERM", "256color-x") ];
  check "256color in the middle" `Ansi_256 [ ("TERM", "a-256color-b") ];
  check "256color alone" `Ansi_256 [ ("TERM", "256color") ];
  check "one character short" `Ansi_16 [ ("TERM", "256colo") ];
  check "case matters in TERM" `Ansi_16 [ ("TERM", "xterm-256COLOR") ];
  check "empty TERM" `Ansi_16 [ ("TERM", "") ];
  check "plain xterm" `Ansi_16 [ ("TERM", "xterm") ]

let test_utf_8 () =
  let check label expected pairs =
    Alcotest.(check bool) label expected (Colour.utf_8 ~getenv:(env pairs))
  in
  check "unset" false [];
  check "all empty" false [ ("LC_ALL", ""); ("LC_CTYPE", ""); ("LANG", "") ];
  check "LANG" true [ ("LANG", "en_US.UTF-8") ];
  check "LC_CTYPE" true [ ("LC_CTYPE", "en_US.UTF-8") ];
  check "LC_ALL" true [ ("LC_ALL", "en_US.UTF-8") ];
  check "no dash" true [ ("LANG", "en_US.UTF8") ];
  check "lower case" true [ ("LANG", "fr_FR.utf-8") ];
  check "mixed case" true [ ("LANG", "fr_FR.Utf8") ];
  check "with a modifier" true [ ("LANG", "ca_ES.UTF-8@valencia") ];
  check "C" false [ ("LANG", "C") ];
  check "POSIX" false [ ("LANG", "POSIX") ];
  check "Latin-1" false [ ("LANG", "fr_FR.ISO-8859-1") ];
  check "UTF-16" false [ ("LANG", "en_US.UTF-16") ];
  check "LC_ALL hides what is UTF-8 below it" false
    [ ("LC_ALL", "C"); ("LC_CTYPE", "C.UTF-8"); ("LANG", "C.UTF-8") ];
  check "LC_ALL hides what is not UTF-8 below it" true
    [ ("LC_ALL", "C.UTF-8"); ("LC_CTYPE", "C"); ("LANG", "C") ];
  check "LC_CTYPE hides LANG" false
    [ ("LC_CTYPE", "C"); ("LANG", "en_US.UTF-8") ];
  check "LANG does not hide LC_CTYPE" true
    [ ("LC_CTYPE", "en_US.UTF-8"); ("LANG", "C") ];
  check "an empty LC_ALL is skipped" true
    [ ("LC_ALL", ""); ("LANG", "en_US.UTF-8") ];
  check "an empty LC_CTYPE is skipped" false [ ("LC_CTYPE", ""); ("LANG", "C") ]

(* Each rule reads the variables it documents and no other: a variable such
   as FORCE_COLOR cannot change what it says. *)
let test_only_documented () =
  let pairs = [ ("TERM", "xterm") ] in
  Alcotest.(check bool)
    "dumb" false
    (Colour.dumb ~getenv:(only [ "TERM" ] pairs));
  Alcotest.check renderer "renderer" `Ansi_tty
    (Colour.renderer ~getenv:(only [ "NO_COLOR"; "TERM" ] pairs) ~is_tty:true);
  Alcotest.check depth "depth" `Ansi_16
    (Colour.depth ~getenv:(only [ "COLORTERM"; "TERM" ] pairs));
  Alcotest.(check bool)
    "utf_8" false
    (Colour.utf_8 ~getenv:(only [ "LC_ALL"; "LC_CTYPE"; "LANG" ] pairs))

(* What the environment holds is not ours: bytes that are not text, control
   characters and very long values are only values that do not match. *)
let test_hostile_values () =
  let long = String.make (1024 * 1024) 'x' in
  let check_depth label expected term =
    Alcotest.check depth label expected
      (Colour.depth ~getenv:(env [ ("TERM", term) ]))
  in
  check_depth "invalid UTF-8" `Ansi_16 "\xff\xfe\xc3";
  check_depth "an escape code" `Ansi_16 "\027[31m\n";
  check_depth "a NUL inside the name" `Ansi_16 "256\000color";
  check_depth "a megabyte of TERM" `Ansi_16 long;
  check_depth "256color after a megabyte" `Ansi_256 (long ^ "256color");
  check_depth "-direct after a megabyte" `True_color (long ^ "-direct");
  let hostile_term = [ ("TERM", "\xff\000\027"); ("NO_COLOR", "\xff") ] in
  Alcotest.check renderer "invalid NO_COLOR is set" `None
    (Colour.renderer ~getenv:(env hostile_term) ~is_tty:true);
  Alcotest.(check bool)
    "invalid TERM is a TERM" false
    (Colour.dumb ~getenv:(env hostile_term));
  let check_utf_8 label expected lang =
    Alcotest.(check bool)
      label expected
      (Colour.utf_8 ~getenv:(env [ ("LANG", lang) ]))
  in
  check_utf_8 "invalid locale" false "\xff\xfe";
  check_utf_8 "a megabyte of locale" false long;
  check_utf_8 "UTF-8 after a megabyte" true (long ^ ".UTF-8");
  check_utf_8 "cut short" false "en_US.UTF-"

let suite =
  ( "colour",
    [
      Alcotest.test_case "dumb" `Quick test_dumb;
      Alcotest.test_case "renderer" `Quick test_renderer;
      Alcotest.test_case "depth" `Quick test_depth;
      Alcotest.test_case "utf_8" `Quick test_utf_8;
      Alcotest.test_case "reads only the variables it documents" `Quick
        test_only_documented;
      Alcotest.test_case "hostile values" `Quick test_hostile_values;
    ] )
