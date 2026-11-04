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

let depth =
  let pp ppf d =
    Fmt.string ppf
      (match d with
      | `True_color -> "`True_color"
      | `Ansi_256 -> "`Ansi_256"
      | `Ansi_16 -> "`Ansi_16")
  in
  Alcotest.testable pp ( = )

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

(* Environment values are untrusted: bytes that are not UTF-8, control
   characters and very long values are read as any other value is, and
   raise nothing. *)
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
      Alcotest.test_case "reads only the variables it documents" `Quick
        test_only_documented;
      Alcotest.test_case "hostile values" `Quick test_hostile_values;
    ] )
