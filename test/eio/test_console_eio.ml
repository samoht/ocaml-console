(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let contains affix s = Astring.String.is_infix ~affix s
let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.clock env)

let buffer () =
  let buf = Buffer.create 64 in
  (buf, Format.formatter_of_buffer buf)

let test_run_returns () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      let result = Console_eio.run ~clock ~ppf (fun _display -> 42) in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "nothing on a pipe" "" (Buffer.contents buf))

let test_run_raises () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun display ->
              ignore (Console.Display.task display "doomed");
              failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the running task is cancelled" true
        (contains "\xe2\x97\x8b doomed" (Buffer.contents buf)))

let test_nested_run_reuses_the_display () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              Console.Display.succeed (Console.Display.task inner "nested work")));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses the outer output" true
        (String.starts_with ~prefix:"\xe2\x9c\x93 nested work"
           (Buffer.contents buf)))

(* A display that is not this fiber's is refused, and the one that runs is
   not disturbed. *)
let test_second_display_refused () =
  with_clock (fun clock ->
      let _, ppf = buffer () in
      let started, start = Eio.Promise.create () in
      let finish, finished = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          Console_eio.run ~clock ~ppf (fun _ ->
              Eio.Promise.resolve start ();
              Eio.Promise.await finish))
        (fun () ->
          Eio.Promise.await started;
          Alcotest.check_raises "refused"
            (Invalid_argument "Console_eio.run: a display is already running")
            (fun () -> Console_eio.run ~clock ~ppf ignore);
          Eio.Promise.resolve finished ());
      Console_eio.run ~clock ~ppf ignore)

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

let test_dimensions_read_the_environment () =
  let check msg columns lines expected =
    with_env
      [ ("COLUMNS", columns); ("LINES", lines) ]
      (fun () ->
        Alcotest.(check (pair int int)) msg expected (Terminal.dimensions None))
  in
  check "the exported size" "123" "40" (123, 40);
  check "a small size" "10" "2" (10, 2);
  check "zero is no answer" "0" "0" (80, 24);
  check "nor is none" "" "" (80, 24)

(* The rules of cmdliner and fmt, and the depth the environment announces. *)
let test_colour_rules () =
  let env pairs name = List.assoc_opt name pairs in
  let renderer ~is_tty pairs =
    match Colour.renderer ~getenv:(env pairs) ~is_tty with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let term = ("TERM", "xterm-256color") in
  let check label expected got = Alcotest.(check string) label expected got in
  check "terminal" "ansi" (renderer ~is_tty:true [ term ]);
  check "pipe" "none" (renderer ~is_tty:false [ term ]);
  check "NO_COLOR" "none" (renderer ~is_tty:true [ term; ("NO_COLOR", "1") ]);
  check "empty NO_COLOR" "ansi"
    (renderer ~is_tty:true [ term; ("NO_COLOR", "") ]);
  check "TERM dumb" "none" (renderer ~is_tty:true [ ("TERM", "dumb") ]);
  check "TERM empty" "none" (renderer ~is_tty:true [ ("TERM", "") ]);
  check "TERM unset" "none" (renderer ~is_tty:true []);
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
  check "nothing" "16" (depth [])

(* [setup]'s override is the program's: it wins over the environment. *)
let test_setup () =
  let restore () =
    Fmt.set_style_renderer Fmt.stdout `None;
    Fmt.set_style_renderer Fmt.stderr `None
  in
  Fun.protect ~finally:restore (fun () ->
      with_env
        [ ("NO_COLOR", "1"); ("TERM", "xterm-256color"); ("COLORTERM", "") ]
        (fun () ->
          Console_eio.setup ~style_renderer:`Ansi_tty ();
          Alcotest.(check bool)
            "--color=always wins over NO_COLOR" true
            (Fmt.style_renderer Fmt.stdout = `Ansi_tty);
          Alcotest.(check bool)
            "the depth of TERM" true
            (Console.Color.depth Fmt.stdout = `Ansi_256);
          Console_eio.setup ();
          Alcotest.(check bool)
            "NO_COLOR" true
            (Fmt.style_renderer Fmt.stdout = `None)))

(* {1 On a terminal} *)

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

(* What [teardown.exe how] writes on a terminal, and how it exits. *)
let teardown how =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:[| "TERM=xterm-256color" |]
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:"./teardown.exe" [ "teardown.exe"; how ]
  in
  let clock = Eio.Stdenv.clock env in
  let written = Buffer.create 256 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let status = ref None in
  (* Read until the child has exited and nothing more comes. *)
  let rec drain () =
    match
      Eio.Time.with_timeout clock 0.5 (fun () ->
          Ok (Eio.Flow.single_read source buf))
    with
    | Ok n ->
        Buffer.add_string written (Cstruct.to_string ~len:n buf);
        drain ()
    | Error `Timeout -> if Option.is_none !status then drain ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  Eio.Fiber.both (fun () -> status := Some (Eio.Process.await child)) drain;
  (Option.get !status, Buffer.contents written)

let hide = "\027[?25l"

let last_cursor s =
  let rec find i acc =
    match Astring.String.find_sub ~start:i ~sub:"\027[?25" s with
    | None -> acc
    | Some j when j + 5 < String.length s -> find (j + 5) (Some s.[j + 5])
    | Some _ -> acc
  in
  find 0 None

let test_teardown () =
  List.iter
    (fun (how, expected) ->
      let status, out = teardown how in
      Alcotest.(check bool) (how ^ ": hid the cursor") true (contains hide out);
      Alcotest.(check (option char))
        (how ^ ": the cursor is shown last")
        (Some 'h') (last_cursor out);
      Alcotest.(check bool)
        (how ^ ": printed above the region")
        true
        (contains "a line of output" out);
      let vte = Vte.create ~rows:24 ~cols:80 () in
      Vte.feed_string vte out;
      Alcotest.(check int)
        (how ^ ": the cursor is at the start of a line")
        0
        (snd (Vte.cursor_pos vte));
      Alcotest.(check bool) (how ^ ": exit status") true (status = expected))
    [
      ("ok", `Exited 0);
      ("exit", `Exited 3);
      ("raise", `Exited 2);
      ("cancel", `Exited 0);
    ]

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "run returns" `Quick test_run_returns;
      Alcotest.test_case "run raises" `Quick test_run_raises;
      Alcotest.test_case "nested run reuses the display" `Quick
        test_nested_run_reuses_the_display;
      Alcotest.test_case "a second display is refused" `Quick
        test_second_display_refused;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "colour rules" `Quick test_colour_rules;
      Alcotest.test_case "setup" `Quick test_setup;
      Alcotest.test_case "teardown on a terminal" `Quick test_teardown;
    ] )
