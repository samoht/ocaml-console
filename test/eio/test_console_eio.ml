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
      Console_eio.run ~clock ~ppf ~mode:`History (fun outer ->
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
      ~is_tty:false ()
  in
  Console_eio.run_on ~ctx ~mode:`History (fun display ->
      let task = Console.Display.task display "step" in
      now := 1.3;
      Console.Display.succeed task);
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "the caller's formatter and the caller's clock" "=> step  1.3s\n"
    (Buffer.contents buf);
  Alcotest.(check (float 0.))
    "the context reads the same clock" 1.3
    (Console.Display.ctx_now ctx)

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
      ~is_tty:false ()
  in
  let read =
    Console_eio.run_on ~ctx ~mode:`History (fun display ->
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

(* A context built without [~wait] can be drawn on but not driven, and a driver
   says so before it draws, rather than refreshing on a clock nobody named. *)
let test_run_on_refuses_a_context_it_cannot_wait_on () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> 0.)
      ~dimensions:(fun () -> (24, 10))
      ~is_tty:false ()
  in
  Alcotest.check_raises "refused"
    (Invalid_argument
       "Console.Display.wait: the context was built without ~wait, so its \
        clock cannot be waited on") (fun () ->
      Console_eio.run_on ~ctx ~mode:`History (fun _display ->
          Alcotest.fail "the body ran under a display with no clock to wait on"))

(* The terminal question, asked of the library rather than of Unix. Under
   [dune test] stdout is a pipe or a file, so the answer is [false]; a runner
   that attached a terminal leaves the case nothing to pin. *)
let test_is_tty_is_false_without_a_terminal () =
  if Unix.isatty Unix.stdout then Alcotest.skip ();
  Alcotest.(check bool)
    "stdout is not a terminal" false
    (Console_eio.is_tty Unix.stdout)

(* Dimensions always answer, whatever the environment: the ioctl, else
   $COLUMNS/$LINES, else the 80x24 default. Ten columns or two rows is no
   answer, and the next source is asked. Under [dune test] stdout is no
   terminal, so the ioctl has nothing to say and the environment decides. *)
let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

let test_dimensions_read_the_environment () =
  if Unix.isatty Unix.stdout then Alcotest.skip ();
  let dims = Alcotest.(pair int int) in
  with_env
    [ ("COLUMNS", "123"); ("LINES", "45") ]
    (fun () ->
      Alcotest.check dims "the exported size" (123, 45)
        (Console_eio.dimensions ()));
  with_env
    [ ("COLUMNS", "11"); ("LINES", "3") ]
    (fun () ->
      Alcotest.check dims "the smallest size taken" (11, 3)
        (Console_eio.dimensions ()));
  with_env
    [ ("COLUMNS", "10"); ("LINES", "2") ]
    (fun () ->
      Alcotest.check dims "a smaller one is no answer" (80, 24)
        (Console_eio.dimensions ()));
  with_env
    [ ("COLUMNS", ""); ("LINES", "") ]
    (fun () ->
      Alcotest.check dims "nor is none" (80, 24) (Console_eio.dimensions ()))

(* An elapsed time comes from the machine, not from the program, so a test that
   captures a display's output cannot assert one. CONSOLE_FROZEN_CLOCK is the
   door that makes such output reproducible: the same work, long enough that a
   moving clock shows it, renders 0.0s with the variable set. Both halves are
   asserted, because a frozen clock that was already frozen proves nothing. The
   clock is a mock the task moves a quarter of a second, so the work takes no
   time on the machine's. *)
let capture_a_slow_task () =
  Eio_main.run @@ fun _env ->
  let mock = Eio_mock.Clock.make () in
  let clock = (mock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Console_eio.run ~clock ~ppf ~mode:`History (fun display ->
      let task = Console.Display.task display "slow step" in
      Eio_mock.Clock.set_time mock 0.25;
      Console.Display.succeed task);
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let ends_with_frozen_elapsed line =
  String.length line > 5 && String.ends_with ~suffix:"  0.0s" line

let test_frozen_clock_pins_every_elapsed () =
  let variable = "CONSOLE_FROZEN_CLOCK" in
  let running = capture_a_slow_task () in
  Alcotest.(check bool)
    "a moving clock times the work" false
    (ends_with_frozen_elapsed (String.trim running));
  Unix.putenv variable "1";
  let frozen =
    Fun.protect ~finally:(fun () -> Unix.putenv variable "") capture_a_slow_task
  in
  Alcotest.(check bool)
    "a frozen clock reads zero" true
    (ends_with_frozen_elapsed (String.trim frozen));
  Alcotest.(check bool)
    "the variable no longer freezes once cleared" false
    (ends_with_frozen_elapsed (String.trim (capture_a_slow_task ())))

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
  let run f = Console_eio.run ~clock:(Eio.Stdenv.clock env) ~mode:`History f in
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

(* A mark no display writes, so a reader of the terminal knows it has read
   everything written before it. *)
let end_mark = "<end of the case's output>"
let ends_with_mark s = String.ends_with ~suffix:end_mark s

(* A history display on a terminal hides no cursor, so it writes nothing to
   show one: what reaches the terminal is what the caller printed, the bytes a
   pipe would get apart from the terminal's own CR before each LF. *)
let test_history_on_a_terminal_writes_only_its_output () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  let stdout = Unix.dup Unix.stdout in
  ( Fun.protect ~finally:(fun () ->
        Unix.dup2 stdout Unix.stdout;
        Unix.close stdout)
  @@ fun () ->
    Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) (fun tty ->
        Unix.dup2 tty Unix.stdout);
    Console_eio.run ~clock:(Eio.Stdenv.clock env) ~mode:`History (fun display ->
        Console.Display.with_output display (fun ppf -> Fmt.pf ppf "plain@."));
    (* The end of the display's output is a mark written after it returns, on
       the same terminal: the pseudoterminal end reads in order, so everything
       before the mark is what the display wrote, with no window of silence to
       wait out. Closing the terminal end instead loses its unread output on
       macOS. *)
    ignore
      (Unix.write_substring Unix.stdout end_mark 0 (String.length end_mark)) );
  let written = Buffer.create 64 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let rec read_to_mark () =
    if not (ends_with_mark (Buffer.contents written)) then (
      let n = Eio.Flow.single_read source buf in
      Buffer.add_string written (Cstruct.to_string ~len:n buf);
      read_to_mark ())
  in
  (match
     Eio.Time.with_timeout (Eio.Stdenv.clock env) 10. (fun () ->
         Ok (read_to_mark ()))
   with
  | Ok () -> ()
  | Error `Timeout -> Alcotest.fail "the end mark was not read in 10 s");
  Alcotest.(check string)
    "only the output" ("plain\r\n" ^ end_mark) (Buffer.contents written)

(* [setup] reads the environment once: NO_COLOR turns stdout's styling off
   whatever it is, and TERM sets the depth colours are written at. *)
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
      let written = Buffer.create 16 in
      let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
      Format.pp_set_formatter_out_functions Fmt.stdout
        {
          saved with
          out_string = Buffer.add_substring written;
          out_flush = ignore;
        };
      Fun.protect
        ~finally:(fun () ->
          Format.pp_set_formatter_out_functions Fmt.stdout saved)
        (fun () ->
          Console_eio.setup ~style_renderer:`Ansi_tty ();
          Fmt.pr "\027[38;2;255;0;0mred\027[0m%!");
      Alcotest.(check bool)
        "--color wins over NO_COLOR" true
        (Fmt.style_renderer Fmt.stdout = `Ansi_tty);
      Alcotest.(check string)
        "depth from TERM" "\027[38;5;196mred\027[0m" (Buffer.contents written);
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
      Alcotest.test_case "run_on refuses a context it cannot wait on" `Quick
        test_run_on_refuses_a_context_it_cannot_wait_on;
      Alcotest.test_case "is_tty is false without a terminal" `Quick
        test_is_tty_is_false_without_a_terminal;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "frozen clock pins every elapsed" `Quick
        test_frozen_clock_pins_every_elapsed;
      Alcotest.test_case "a handler installed under run outlives it" `Quick
        test_a_handler_installed_under_run_outlives_it;
      Alcotest.test_case "history on a terminal writes only its output" `Quick
        test_history_on_a_terminal_writes_only_its_output;
    ] )

module Frame = struct
  (* Console_eio.frame_drawn: the promise a fiber awaits to know that frame n of
     a display is on the terminal. The clocks here never move: what resolves the
     promise is the paint of the frame, so no case reads or waits on time. The
     scene draws its frame number, which is how a case reads what the terminal
     holds when the promise resolves. *)

  module D = Console.Display

  let resolved p = Option.is_some (Eio.Promise.peek p)

  (* A context on [buf] whose clock reads zero for ever; [wait] is how the
     driver of [Console_eio.run_on] paces its refresh. *)
  let context ?wait buf =
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `None;
    ( ppf,
      D.ctx ~ppf
        ~now:(fun () -> 0.)
        ?wait
        ~dimensions:(fun () -> (40, 10))
        ~is_tty:true () )

  let scene ~frame ~width:_ = [ Fmt.kstr Console.Span.text "frame %d" frame ]

  let on_screen ppf buf n =
    Format.pp_print_flush ppf ();
    Astring.String.is_infix ~affix:(Fmt.str "frame %d" n) (Buffer.contents buf)

  (* Display.on_frame: the promise for frame 2 is pending through the paint of
     frame 1 and resolved by the paint of frame 2, which is then on the
     terminal. *)
  let test_resolved_by_the_paint_of_its_frame () =
    Eio_main.run @@ fun _env ->
    let buf = Buffer.create 1024 in
    let ppf, ctx = context buf in
    D.run ~ctx ~mode:`Activity (fun d ->
        D.set_scene d scene;
        let p = Console_eio.frame_drawn d 2 in
        Alcotest.(check bool) "pending at frame 0" false (resolved p);
        D.tick d;
        Alcotest.(check bool) "pending at frame 1" false (resolved p);
        Alcotest.(check bool)
          "frame 2 not drawn yet" false (on_screen ppf buf 2);
        D.tick d;
        Alcotest.(check bool) "resolved at frame 2" true (resolved p);
        Alcotest.(check bool)
          "frame 2 on the terminal" true (on_screen ppf buf 2);
        Alcotest.(check bool)
          "the frame on the terminal resolves at once" true
          (resolved (Console_eio.frame_drawn d 2));
        Alcotest.(check bool)
          "a frame drawn before it resolves at once" true
          (resolved (Console_eio.frame_drawn d 1)))

  (* A tick while the region is lent to [with_output] moves the frame and paints
     nothing; the frame is drawn by the paint that gives the region back. *)
  let test_a_lent_region_draws_on_its_return () =
    Eio_main.run @@ fun _env ->
    let buf = Buffer.create 1024 in
    let ppf, ctx = context buf in
    D.run ~ctx ~mode:`Activity (fun d ->
        D.set_scene d scene;
        let p = Console_eio.frame_drawn d 1 in
        D.with_output d (fun _ ->
            D.tick d;
            Alcotest.(check int) "the frame moved" 1 (D.frame d);
            Alcotest.(check bool) "pending while lent" false (resolved p));
        Alcotest.(check bool) "resolved on return" true (resolved p);
        Alcotest.(check bool)
          "frame 1 on the terminal" true (on_screen ppf buf 1))

  (* An appending display draws no frame, so there is none to wait for. *)
  let test_an_appending_display_resolves_at_once () =
    Eio_main.run @@ fun _env ->
    let buf = Buffer.create 64 in
    let _, ctx = context buf in
    D.run ~ctx ~mode:`History (fun d ->
        Alcotest.(check bool)
          "resolved" true
          (resolved (Console_eio.frame_drawn d 5)))

  (* A frame still to come when the display closes is never drawn, and its
     promise is resolved as the display closes, so no fiber waits on it for
     ever. *)
  let test_closing_resolves_what_is_to_come () =
    Eio_main.run @@ fun _env ->
    let buf = Buffer.create 1024 in
    let _, ctx = context buf in
    let p =
      D.run ~ctx ~mode:`Activity (fun d ->
          let p = Console_eio.frame_drawn d 10 in
          Alcotest.(check bool) "pending while open" false (resolved p);
          p)
    in
    Alcotest.(check bool) "resolved once closed" true (resolved p)

  (* Under the Eio driver, a fiber awaits frame 3 while the refresh turns on a
     clock that never moves: the await ends on the refresh that draws frame 3,
     and the terminal then holds it. The wait is bounded in refreshes, not in
     time: a refresh past the eighth means the await missed its frame. *)
  exception Missed_the_frame

  let test_await_under_the_driver () =
    Eio_main.run @@ fun _env ->
    let buf = Buffer.create 4096 in
    let refreshes = ref 0 in
    let wait _ =
      incr refreshes;
      if !refreshes > 8 then raise Missed_the_frame;
      Eio.Fiber.yield ()
    in
    let ppf, ctx = context ~wait buf in
    Console_eio.run_on ~ctx ~mode:`Activity (fun d ->
        D.set_scene d scene;
        Eio.Promise.await (Console_eio.frame_drawn d 3);
        Alcotest.(check int) "the frame drawn" 3 (D.frame d);
        Alcotest.(check bool)
          "frame 3 on the terminal" true (on_screen ppf buf 3))

  let suite =
    ( "frame",
      [
        Alcotest.test_case "resolved by the paint of its frame" `Quick
          test_resolved_by_the_paint_of_its_frame;
        Alcotest.test_case "a lent region draws on its return" `Quick
          test_a_lent_region_draws_on_its_return;
        Alcotest.test_case "an appending display resolves at once" `Quick
          test_an_appending_display_resolves_at_once;
        Alcotest.test_case "closing resolves what is to come" `Quick
          test_closing_resolves_what_is_to_come;
        Alcotest.test_case "await under the driver" `Quick
          test_await_under_the_driver;
      ] )
end

let frame_suite = Frame.suite
