(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Env_harness

(* The rules of cmdliner and fmt, and the depth the environment announces. *)
let test_colour_rules () =
  let env pairs name = List.assoc_opt name pairs in
  let renderer ~is_tty pairs =
    match Colour.renderer ~getenv:(env pairs) ~is_tty with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let term = ("TERM", "xterm-256color") in
  let check = Alcotest.(check string) in
  check "terminal" "ansi" (renderer ~is_tty:true [ term ]);
  check "pipe" "none" (renderer ~is_tty:false [ term ]);
  check "NO_COLOR" "none" (renderer ~is_tty:true [ term; ("NO_COLOR", "1") ]);
  check "empty NO_COLOR" "ansi"
    (renderer ~is_tty:true [ term; ("NO_COLOR", "") ]);
  check "TERM dumb" "none" (renderer ~is_tty:true [ ("TERM", "dumb") ]);
  check "TERM empty" "none" (renderer ~is_tty:true [ ("TERM", "") ]);
  check "TERM unset" "none" (renderer ~is_tty:true []);
  check "CLICOLOR_FORCE is not read" "none"
    (renderer ~is_tty:false [ term; ("CLICOLOR_FORCE", "1") ]);
  check "FORCE_COLOR is not read" "none"
    (renderer ~is_tty:false [ term; ("FORCE_COLOR", "1") ]);
  let depth pairs =
    match Colour.depth ~getenv:(env pairs) with
    | `True_color -> "24"
    | `Ansi_256 -> "256"
    | `Ansi_16 -> "16"
  in
  check "COLORTERM truecolor" "24"
    (depth [ ("COLORTERM", "truecolor"); ("TERM", "xterm") ]);
  check "COLORTERM 24bit" "24" (depth [ ("COLORTERM", "24bit") ]);
  check "TERM direct" "24" (depth [ ("TERM", "xterm-direct") ]);
  check "TERM 256color" "256" (depth [ ("TERM", "screen-256color") ]);
  check "other COLORTERM" "256"
    (depth [ ("COLORTERM", "yes"); ("TERM", "xterm-256color") ]);
  check "plain xterm" "16" (depth [ ("TERM", "xterm") ]);
  check "nothing" "16" (depth []);
  let utf_8 pairs = Colour.utf_8 ~getenv:(env pairs) in
  let check_utf_8 label expected pairs =
    Alcotest.(check bool) label expected (utf_8 pairs)
  in
  check_utf_8 "LANG" true [ ("LANG", "en_US.UTF-8") ];
  check_utf_8 "lower case" true [ ("LANG", "fr_FR.utf8") ];
  check_utf_8 "C" false [ ("LANG", "C") ];
  check_utf_8 "unset" false [];
  check_utf_8 "LC_ALL first" false
    [ ("LC_ALL", "POSIX"); ("LANG", "en_US.UTF-8") ];
  check_utf_8 "empty LC_ALL skipped" true
    [ ("LC_ALL", ""); ("LC_CTYPE", "C.UTF-8") ]

(* [setup]'s arguments are the program's: they win over the environment. *)
let test_setup () =
  let restore () =
    Fmt.set_style_renderer Fmt.stdout `None;
    Fmt.set_style_renderer Fmt.stderr `None
  in
  Fun.protect ~finally:restore (fun () ->
      with_env
        [
          ("NO_COLOR", "1");
          ("TERM", "xterm-256color");
          ("COLORTERM", "");
          ("LC_ALL", "en_US.UTF-8");
        ]
        (fun () ->
          Console_tty.setup ~style_renderer:`Ansi_tty ~utf_8:false ();
          Alcotest.(check bool)
            "--color=always wins over NO_COLOR" true
            (Fmt.style_renderer Fmt.stdout = `Ansi_tty);
          Alcotest.(check bool)
            "utf_8 wins over the locale" false (Fmt.utf_8 Fmt.stderr);
          Alcotest.(check bool)
            "the depth of TERM" true
            (Console.Color.depth Fmt.stdout = `Ansi_256);
          Console_tty.setup ();
          Alcotest.(check bool)
            "NO_COLOR" true
            (Fmt.style_renderer Fmt.stdout = `None);
          Alcotest.(check bool) "the locale" true (Fmt.utf_8 Fmt.stderr)))

(* [Env.setup] applies the rules of [Console_tty.setup] to any formatter,
   from the terminal's answer it is given rather than one it asks Unix:
   so a formatter on a buffer, which no descriptor backs, gets colour when
   the caller says it is a terminal. *)
let test_env_setup () =
  let renderer ppf =
    match Fmt.style_renderer ppf with `Ansi_tty -> "ansi" | `None -> "none"
  in
  with_env
    [
      ("NO_COLOR", "");
      ("CONSOLE_CAST", "");
      ("TERM", "xterm-256color");
      ("COLORTERM", "");
      ("LC_ALL", "");
      ("LC_CTYPE", "C.UTF-8");
      ("LANG", "C");
    ]
    (fun () ->
      let ppf = Format.formatter_of_buffer (Buffer.create 16) in
      Console.Env.setup ~is_tty:true ppf;
      Alcotest.(check string) "a terminal" "ansi" (renderer ppf);
      Alcotest.(check bool) "LC_CTYPE before LANG" true (Fmt.utf_8 ppf);
      Alcotest.(check bool)
        "the depth of TERM" true
        (Console.Color.depth ppf = `Ansi_256);
      let ppf = Format.formatter_of_buffer (Buffer.create 16) in
      Console.Env.setup ~is_tty:false ppf;
      Alcotest.(check string) "not a terminal" "none" (renderer ppf);
      let ppf = Format.formatter_of_buffer (Buffer.create 16) in
      Console.Env.ensure ~is_tty:true ppf;
      Alcotest.(check string) "nothing chosen" "ansi" (renderer ppf);
      let ppf = Format.formatter_of_buffer (Buffer.create 16) in
      Fmt.set_style_renderer ppf `None;
      Console.Env.ensure ~is_tty:true ppf;
      Alcotest.(check string) "a choice is kept" "none" (renderer ppf))

(* [setup ~theme] gives both outputs the theme, and [setup] without one
   keeps it. *)
let test_setup_theme () =
  let theme ppf = Console.Theme.name (Console.Theme.get ppf) in
  let restore () =
    List.iter
      (fun ppf ->
        Fmt.set_style_renderer ppf `None;
        Console.Theme.set ppf Console.Theme.default)
      [ Fmt.stdout; Fmt.stderr ]
  in
  Fun.protect ~finally:restore (fun () ->
      Console_tty.setup ~theme:Console.Theme.nord ();
      Alcotest.(check string) "standard output" "nord" (theme Fmt.stdout);
      Alcotest.(check string) "standard error" "nord" (theme Fmt.stderr);
      Console_tty.setup ();
      Alcotest.(check string) "kept" "nord" (theme Fmt.stderr))

(* What [probe.exe mode] prints, in an environment that announces 256
   colours. *)
let probe mode =
  let env =
    Array.append
      [| "TERM=xterm-256color"; "COLORTERM=" |]
      (Array.of_list
         (List.filter
            (fun b ->
              not
                (String.starts_with ~prefix:"TERM=" b
                || String.starts_with ~prefix:"COLORTERM=" b))
            (Array.to_list (Unix.environment ()))))
  in
  let ((ic, _, _) as p) =
    Unix.open_process_args_full "./probe.exe" [| "probe"; mode |] env
  in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_full p);
  out

(* [ensure] decides for an output on which the program chose nothing, a
   theme included, and keeps a choice the program made. *)
let test_ensure () =
  Alcotest.(check string) "nothing chosen" "none 256" (probe "plain");
  Alcotest.(check string) "a theme is not a choice" "none 256" (probe "themed");
  Alcotest.(check string) "a choice is kept" "ansi 16" (probe "chosen")

(* A pipe is not a terminal a program can redraw on. *)
let test_is_terminal () =
  let r, w = Unix.pipe () in
  Fun.protect
    ~finally:(fun () ->
      Unix.close r;
      Unix.close w)
    (fun () ->
      with_env
        [ ("TERM", "xterm") ]
        (fun () ->
          Alcotest.(check bool) "a pipe" false (Console_tty.is_terminal w)))

let suite =
  ( "console_tty",
    [
      Alcotest.test_case "colour rules" `Quick test_colour_rules;
      Alcotest.test_case "setup" `Quick test_setup;
      Alcotest.test_case "Env.setup takes the terminal's answer" `Quick
        test_env_setup;
      Alcotest.test_case "setup with a theme" `Quick test_setup_theme;
      Alcotest.test_case "ensure" `Quick test_ensure;
      Alcotest.test_case "is_terminal" `Quick test_is_terminal;
    ] )
