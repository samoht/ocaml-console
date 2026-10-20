(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.mono_clock env)

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

let seconds s = Mtime.of_uint64_ns (Int64.of_float (s *. 1e9))

(* What a task that waits two seconds on a mock monotonic clock prints. *)
let nap () =
  Eio_main.run @@ fun _ ->
  let clock = Eio_mock.Clock.Mono.make () in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Display.with_task display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () -> Eio.Time.Mono.sleep clock 2.)
            (fun () -> Eio_mock.Clock.Mono.set_time clock (seconds 2.))));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let took s out = contains ("\u{2713} nap  " ^ s) out

(* A display reads the time on its monotonic clock. *)
let test_run_on_a_monotonic_clock () =
  Alcotest.(check bool) "the time it took" true (took "2.0s" (nap ()))

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

(* CONSOLE_FROZEN_CLOCK, set and not empty, stops the display's clock at
   zero; the waits still run on [clock]. *)
let test_frozen_clock () =
  Alcotest.(check bool) "a moving clock" true (took "2.0s" (nap ()));
  let frozen = with_env [ ("CONSOLE_FROZEN_CLOCK", "1") ] (fun () -> nap ()) in
  Alcotest.(check bool) "a frozen clock" true (took "0.0s" frozen);
  let empty = with_env [ ("CONSOLE_FROZEN_CLOCK", "") ] (fun () -> nap ()) in
  Alcotest.(check bool) "an empty value" true (took "2.0s" empty)

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
        (String.starts_with
           ~prefix:"\xe2\x80\xa2 nested work\n\xe2\x9c\x93 nested work"
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

(* {1 On a terminal} *)

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

(* What [teardown.exe how] writes on a terminal, how it exits, and whether
   the terminal echoes what is typed once it has. *)
let session ?(no_color = false) ?(lang = "en_US.UTF-8") how =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:
        [|
          "TERM=xterm-256color";
          ("NO_COLOR=" ^ if no_color then "1" else "");
          "LANG=" ^ lang;
        |]
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
  let echo = Eio_unix.Fd.use_exn "tcgetattr" tty Unix.tcgetattr in
  (Option.get !status, Buffer.contents written, echo.Unix.c_echo)

let teardown how =
  let status, out, _ = session how in
  (status, out)

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
      let vte = Vte.create ~rows:24 ~cols:80 () in
      Vte.feed_string vte out;
      Alcotest.(check bool)
        (how ^ ": standard output above the region, unlabelled")
        true
        (List.mem "a line of output" (Console_vte.screen ~scrollback:true vte));
      Alcotest.(check int)
        (how ^ ": the cursor is at the start of a line")
        0
        (snd (Vte.cursor_pos vte));
      Alcotest.(check bool)
        (how ^ ": exit status") true
        (match (status, expected) with
        | `Exited a, `Exited b -> Int.equal a b
        | _ -> false))
    [
      ("ok", `Exited 0);
      ("exit", `Exited 3);
      ("raise", `Exited 2);
      ("cancel", `Exited 0);
    ]

(* The final screen of [teardown.exe how]. *)
let final how =
  let _, out = teardown how in
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte out;
  Console_vte.screen ~scrollback:true vte

(* Ctrl-C cancels the task, which keeps its window under its line; a task
   that succeeds drops its output. *)
let test_sigint_window () =
  let rows = final "sigint" in
  let rec under = function
    | l :: next :: _ when contains "\u{25cb} pushing" l -> next
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no cancelled line"
  in
  Alcotest.(check string)
    "the window stays" "  \u{203a} pushed 3 objects" (under rows);
  Alcotest.(check bool)
    "a success drops it" false
    (List.exists (contains "pushed 3 objects") (final "ok"))

(* A Logs message from a fiber that a task's function forked is kept, after
   the task's label, while the task's output goes with its success. *)
let test_forked_logs () =
  let rows = final "logs" in
  Alcotest.(check bool)
    "the forked message" true
    (List.mem "  ! work: a forked note" rows);
  Alcotest.(check bool)
    "the output dropped" false
    (List.exists (contains "an output line") rows)

let test_sigint_teardown () =
  let _, out = teardown "sigint" in
  Alcotest.(check bool)
    "SIGINT interrupted a live display" true (contains hide out);
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte out;
  Alcotest.(check bool)
    "SIGINT restores the cursor" true (Vte.cursor_visible vte)

(* However a session ends, text written after it is drawn as on a fresh
   terminal: at the first column, in the terminal's own colours. *)
let test_usable_after () =
  List.iter
    (fun (how, no_color) ->
      let _, out, _ = session ~no_color how in
      let name = Fmt.str "%s%s" how (if no_color then " (NO_COLOR)" else "") in
      let vte = Vte.create ~rows:24 ~cols:80 () in
      Vte.feed_string vte out;
      Alcotest.(check bool)
        (name ^ ": cursor shown") true (Vte.cursor_visible vte);
      let y, x = Vte.cursor_pos vte in
      Alcotest.(check int) (name ^ ": first column") 0 x;
      Vte.feed_string vte "AFTER";
      let plain = Vte.create ~rows:1 ~cols:80 () in
      Vte.feed_string plain "AFTER";
      let g = Vte.grid vte and reference = Vte.grid plain in
      let i = Grid.idx g ~x:0 ~y and j = Grid.idx reference ~x:0 ~y:0 in
      Alcotest.(check (list (float 0.)))
        (name ^ ": default colours")
        [
          Grid.get_fg_r reference j;
          Grid.get_fg_g reference j;
          Grid.get_fg_b reference j;
          Grid.get_bg_r reference j;
        ]
        [
          Grid.get_fg_r g i;
          Grid.get_fg_g g i;
          Grid.get_fg_b g i;
          Grid.get_bg_r g i;
        ])
    (List.concat_map
       (fun how -> [ (how, false); (how, true) ])
       [ "ok"; "raise"; "cancel"; "exit"; "sigint" ])

(* A terminal whose locale is not UTF-8 shows other characters as garbage:
   every widget and the display are then drawn with ASCII alone. *)
let test_ascii_locale () =
  let _, out, _ = session ~lang:"C" "widgets" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool)
    "ASCII only" true
    (String.for_all (fun c -> Char.code c < 0x80) out)

(* A program that chose its colours before [run] keeps them. *)
let test_choice_kept () =
  let _, out, _ = session "colourless" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool) "no colour" false (contains "\027[0m" out)

(* A theme is not a colour choice: a program that only chose a theme still
   has colour decided for it, and in that theme. *)
let test_theme_not_a_choice () =
  let _, out, _ = session "themed" in
  let nord =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Console.Color.set_depth ppf `Ansi_256;
    Console.(
      Span.pp ppf (Span.styled Style.(bold + fg (Theme.ok Theme.nord)) "x"));
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    String.sub s 0 (String.index s 'x')
  in
  Alcotest.(check bool) "colour, in the theme" true (contains nord out)

let test_sigint_secret () =
  let status, _, echo = session "secret" in
  Alcotest.(check bool)
    "SIGINT still ends the program" true
    (match status with `Signaled s -> Int.equal s Sys.sigint | _ -> false);
  Alcotest.(check bool) "SIGINT during a password restores echo" true echo

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "the terminal is usable after every ending" `Quick
        test_usable_after;
      Alcotest.test_case "the program's colour choice is kept" `Quick
        test_choice_kept;
      Alcotest.test_case "a theme is not a colour choice" `Quick
        test_theme_not_a_choice;
      Alcotest.test_case "ASCII on a terminal that is not UTF-8" `Quick
        test_ascii_locale;
      Alcotest.test_case "SIGINT during a password restores echo" `Quick
        test_sigint_secret;
      Alcotest.test_case "SIGINT restores the terminal cursor" `Quick
        test_sigint_teardown;
      Alcotest.test_case "SIGINT keeps the window of a cancelled task" `Quick
        test_sigint_window;
      Alcotest.test_case "Logs from a forked fiber on a terminal" `Quick
        test_forked_logs;
      Alcotest.test_case "run returns" `Quick test_run_returns;
      Alcotest.test_case "run on a monotonic clock" `Quick
        test_run_on_a_monotonic_clock;
      Alcotest.test_case "frozen clock" `Quick test_frozen_clock;
      Alcotest.test_case "run raises" `Quick test_run_raises;
      Alcotest.test_case "nested run reuses the display" `Quick
        test_nested_run_reuses_the_display;
      Alcotest.test_case "a second display is refused" `Quick
        test_second_display_refused;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "teardown on a terminal" `Quick test_teardown;
    ] )
