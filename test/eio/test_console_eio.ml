let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.clock env)

let test_run_returns_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      let result = Console_eio.run ~clock ~ppf (fun _display -> 42) in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

let test_run_exception_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun _display -> failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

let test_nested_run_reuses_the_existing_display () =
  with_clock (fun clock ->
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      Console_eio.run ~clock ~ppf (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              let task = Console.Display.task inner "nested work" in
              Console.Display.succeed task));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses outer output" true
        (String.starts_with ~prefix:"=> nested work" (Buffer.contents buf)))

(* A caller that grades its own live region hands [run_on] the context
   instead of letting [run] build one. What proves the handed context was
   used, rather than rebuilt from stdout and the machine clock, is the elapsed
   time: no real clock reads 1.3s over work that takes none, and no default
   formatter puts the line in this buffer. *)
let test_run_on_takes_the_callers_context () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~wait:(fun _ -> Eio.Fiber.await_cancel ())
      ~dimensions:(fun () -> (24, 10))
      ~live:false ()
  in
  Console_eio.run_on ~ctx (fun display ->
      let task = Console.Display.task display "step" in
      now := 1.3;
      Console.Display.succeed task);
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "the caller's formatter and the caller's clock" "=> step  1.3s\n"
    (Buffer.contents buf)

(* The context's clock is the display's only one: the refresh waits through
   the context, and what the display reads as now is that clock's reading. The
   clock here advances only when it is waited on, so a refresh paced by any
   other clock leaves it at zero and records no wait. *)
let test_run_on_waits_on_the_contexts_clock () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let waits = ref [] in
  let wait seconds =
    waits := seconds :: !waits;
    now := !now +. seconds;
    Eio.Fiber.yield ()
  in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~wait
      ~dimensions:(fun () -> (24, 10))
      ~live:false ()
  in
  let read =
    Console_eio.run_on ~ctx (fun display ->
        for _ = 1 to 3 do
          Eio.Fiber.yield ()
        done;
        Console.Display.now display)
  in
  Alcotest.(check (list (float 0.)))
    "the refresh waited on the context's clock, once before the body and once \
     on each of its yields"
    [ 0.12; 0.12; 0.12; 0.12 ] (List.rev !waits);
  Alcotest.(check (float 0.))
    "the display reads that clock"
    (0.12 +. 0.12 +. 0.12 +. 0.12)
    read

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

(* The width of a display on a descriptor with no terminal behind it. *)
let width () =
  Eio_main.run @@ fun env ->
  let ppf = Format.formatter_of_buffer (Buffer.create 16) in
  Console_eio.run ~clock:(Eio.Stdenv.clock env) ~ppf (fun display ->
      Console.Display.with_output display (fun ppf ->
          Format.pp_get_margin ppf ()))

let test_dimensions_read_the_environment () =
  if Unix.isatty Unix.stdout then Alcotest.skip ();
  let check msg columns expected =
    with_env
      [ ("COLUMNS", columns) ]
      (fun () -> Alcotest.(check int) msg expected (width ()))
  in
  check "the exported width" "123" 123;
  check "a narrow width taken" "10" 10;
  check "zero is no answer" "0" 80;
  check "nor is none" "" 80

(* Under a terminal [run] takes SIGINT, to give the cursor back before the
   process dies of it, and on return gives back the handler it displaced. A
   program whose Ctrl-C is its own affair installs its handler under [run]
   and keeps it past [run]'s return, as a run that must answer a Ctrl-C after
   its display has closed does: what [run] restores is the handler it found,
   and only where its own still stands. The terminal is a pseudo-terminal put
   on stdout for the span of the case. *)
let handler_installed_under_run (_ : int) = ()
let handler_installed_before_run (_ : int) = ()

let is handler = function
  | Sys.Signal_handle f -> f == handler
  | Sys.Signal_default | Sys.Signal_ignore -> false

let test_a_handler_installed_under_run_outlives_it () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  (* What the display draws is read and dropped, so no write to the terminal
     blocks on a full buffer. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Cstruct.create 4096 in
      let source = Eio_unix.Pty.source pty in
      (try
         while true do
           ignore (Eio.Flow.single_read source buf : int)
         done
       with End_of_file | Eio.Io _ -> ());
      `Stop_daemon);
  let stdout = Unix.dup Unix.stdout in
  let before = Sys.signal Sys.sigint Sys.Signal_default in
  Sys.set_signal Sys.sigint before;
  let before_term = Sys.signal Sys.sigterm Sys.Signal_default in
  Sys.set_signal Sys.sigterm before_term;
  Fun.protect ~finally:(fun () ->
      Unix.dup2 stdout Unix.stdout;
      Unix.close stdout;
      Sys.set_signal Sys.sigint before;
      Sys.set_signal Sys.sigterm before_term)
  @@ fun () ->
  Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) (fun tty ->
      Unix.dup2 tty Unix.stdout);
  let run f = Console_eio.run ~clock:(Eio.Stdenv.clock env) f in
  (* The control: [run] takes the terminal, installs a handler of its own and,
     where nothing replaced it, gives back the one it found. *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler_installed_before_run);
  run (fun _display ->
      match Sys.signal Sys.sigint Sys.Signal_default with
      | Sys.Signal_handle h as own when h != handler_installed_before_run ->
          Sys.set_signal Sys.sigint own
      | Sys.Signal_handle _ | Sys.Signal_default | Sys.Signal_ignore ->
          Alcotest.fail "run installed no handler: stdout is not a terminal");
  (* A TERM, as [kill] and a supervisor send, ends the process as a Ctrl-C
     does, and gives the cursor back first the same way. *)
  run (fun _display ->
      match Sys.signal Sys.sigterm Sys.Signal_default with
      | Sys.Signal_handle _ as own -> Sys.set_signal Sys.sigterm own
      | Sys.Signal_default | Sys.Signal_ignore ->
          Alcotest.fail "run installed no SIGTERM handler");
  Alcotest.(check bool)
    "the TERM disposition run found is back once run returns" true
    (match Sys.signal Sys.sigterm Sys.Signal_default with
    | Sys.Signal_default -> true
    | Sys.Signal_handle _ | Sys.Signal_ignore -> false);
  Alcotest.(check bool)
    "the handler run found is back once run returns" true
    (is handler_installed_before_run (Sys.signal Sys.sigint Sys.Signal_default));
  run (fun _display ->
      Sys.set_signal Sys.sigint (Sys.Signal_handle handler_installed_under_run));
  Alcotest.(check bool)
    "the handler installed under run still stands after it" true
    (is handler_installed_under_run (Sys.signal Sys.sigint Sys.Signal_default))

(* [setup] reads the environment once: NO_COLOR turns stdout's styling off
   unless --color says otherwise. *)
let test_setup_reads_the_environment () =
  let saved name = (name, Sys.getenv_opt name) in
  let env = List.map saved [ "NO_COLOR"; "TERM"; "COLORTERM" ] in
  let restore () =
    List.iter
      (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
      env;
    Fmt.set_style_renderer Fmt.stdout `None;
    Fmt.set_style_renderer Fmt.stderr `None
  in
  Fun.protect ~finally:restore (fun () ->
      Unix.putenv "NO_COLOR" "1";
      Unix.putenv "TERM" "xterm-256color";
      Unix.putenv "COLORTERM" "";
      Console_eio.setup ~style_renderer:`Ansi_tty ();
      Alcotest.(check bool)
        "--color wins over NO_COLOR" true
        (Fmt.style_renderer Fmt.stdout = `Ansi_tty);
      Console_eio.setup ();
      Alcotest.(check bool)
        "NO_COLOR" true
        (Fmt.style_renderer Fmt.stdout = `None))

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "setup reads the environment" `Quick
        test_setup_reads_the_environment;
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "nested run reuses the existing display" `Quick
        test_nested_run_reuses_the_existing_display;
      Alcotest.test_case "run_on takes the caller's context" `Quick
        test_run_on_takes_the_callers_context;
      Alcotest.test_case "run_on waits on the context's clock" `Quick
        test_run_on_waits_on_the_contexts_clock;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "a handler installed under run outlives it" `Quick
        test_a_handler_installed_under_run_outlives_it;
    ] )
