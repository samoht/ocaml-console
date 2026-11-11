(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let restore f =
  let before = Console.Theme.program () in
  Fun.protect f ~finally:(fun () ->
      Console.Theme.set_program before;
      List.iter
        (fun ppf -> Fmt.set_style_renderer ppf `None)
        [ Fmt.stdout; Fmt.stderr ])

(* [run args] runs the command with [args]. It returns the exit code, whether
   the command ran, and the program's theme. *)
let run args =
  let ran = ref false in
  let cmd =
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "t")
      Cmdliner.Term.(const (fun () -> ran := true) $ Console_cli.setup ())
  in
  let null = Format.formatter_of_buffer (Buffer.create 0) in
  let code =
    Cmdliner.Cmd.eval ~help:null ~err:null
      ~argv:(Array.of_list ("t" :: args))
      cmd
  in
  (code, !ran, Console.Theme.name (Console.Theme.program ()))

let test_theme () =
  restore @@ fun () ->
  let code, ran, theme = run [ "--theme=dos" ] in
  Alcotest.(check int) "exit" 0 code;
  Alcotest.(check bool) "ran" true ran;
  Alcotest.(check string) "the program's theme" "dos" theme;
  Alcotest.(check string)
    "standard error" "dos"
    (Console.Theme.name (Console.Theme.get Fmt.stderr))

let test_no_theme () =
  restore @@ fun () ->
  Console.Theme.set_program Console.Theme.nord;
  let _, ran, theme = run [] in
  Alcotest.(check bool) "ran" true ran;
  Alcotest.(check string) "kept" "nord" theme

let test_unknown_theme () =
  restore @@ fun () ->
  let code, ran, _ = run [ "--theme=nope" ] in
  Alcotest.(check bool) "refused" true (code <> 0);
  Alcotest.(check bool) "did not run" false ran

let test_color () =
  restore @@ fun () ->
  let _ = run [ "--color=always" ] in
  Alcotest.(check bool)
    "always" true
    (Fmt.style_renderer Fmt.stdout = `Ansi_tty
    && Fmt.style_renderer Fmt.stderr = `Ansi_tty);
  let _ = run [ "--color=never" ] in
  Alcotest.(check bool)
    "never" true
    (Fmt.style_renderer Fmt.stdout = `None
    && Fmt.style_renderer Fmt.stderr = `None)

let () =
  Alcotest.run "console.cli"
    [
      ( "setup",
        [
          Alcotest.test_case "--theme" `Quick test_theme;
          Alcotest.test_case "no --theme" `Quick test_no_theme;
          Alcotest.test_case "an unknown theme" `Quick test_unknown_theme;
          Alcotest.test_case "--color" `Quick test_color;
        ] );
    ]
