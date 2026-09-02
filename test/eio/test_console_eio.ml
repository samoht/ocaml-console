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

(* A caller that grades its own live region hands [run] the context instead of
   letting it build one. What proves the handed context was used, rather than
   rebuilt from stdout and the machine clock, is the elapsed time: no real clock
   reads 1.3s over work that takes none, and no default formatter puts the line
   in this buffer. *)
let test_run_takes_the_callers_context () =
  with_clock (fun clock ->
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      let now = ref 0. in
      let ctx =
        Console.Display.ctx ~ppf
          ~now:(fun () -> !now)
          ~dimensions:(fun () -> (24, 10))
          ~is_tty:false ()
      in
      Console_eio.run ~clock ~ctx ~mode:`History (fun display ->
          let task = Console.Display.task display "step" in
          now := 1.3;
          Console.Display.succeed task);
      Format.pp_print_flush ppf ();
      Alcotest.(check string)
        "the caller's formatter and the caller's clock" "=> step  1.3s\n"
        (Buffer.contents buf))

(* A context already carries a formatter, so a second one beside it is a
   contradiction rather than a preference to resolve quietly. *)
let test_run_refuses_a_formatter_beside_a_context () =
  with_clock (fun clock ->
      let buf = Buffer.create 16 in
      let ppf = Format.formatter_of_buffer buf in
      let ctx = Console.Display.dumb () in
      Alcotest.check_raises "refused"
        (Invalid_argument
           "Console_eio.run: ppf and ctx are both given; a context already \
            carries its formatter") (fun () ->
          Console_eio.run ~clock ~ppf ~ctx (fun _display -> ()));
      Format.pp_print_flush ppf ();
      Alcotest.(check string) "and nothing was drawn" "" (Buffer.contents buf))

(* The terminal question, asked of the library rather than of Unix. Under
   [dune test] stdout is a pipe or a file, so the answer is [false]; a runner
   that attached a terminal leaves the case nothing to pin. *)
let test_is_tty_is_false_without_a_terminal () =
  if Unix.isatty Unix.stdout then Alcotest.skip ();
  Alcotest.(check bool) "stdout is not a terminal" false (Console_eio.is_tty ())

(* Dimensions always answer, whatever the environment: the ioctl, else
   $COLUMNS/$LINES, else the 80x24 default. No source of an answer can make
   either component smaller than a single cell. *)
let test_dimensions_are_at_least_one_cell () =
  let width, height = Console_eio.dimensions () in
  Alcotest.(check bool) "width is at least one cell" true (width >= 1);
  Alcotest.(check bool) "height is at least one cell" true (height >= 1)

(* An elapsed time comes from the machine, not from the program, so a test that
   captures a display's output cannot assert one. CONSOLE_FROZEN_CLOCK is the
   door that makes such output reproducible: the same work, slow enough that a
   real clock would show it, renders 0.0s with the variable set. Both halves are
   asserted, because a frozen clock that was already frozen proves nothing. *)
let capture_a_slow_task () =
  with_clock (fun clock ->
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      Console_eio.run ~clock ~ppf ~mode:`History (fun display ->
          let task = Console.Display.task display "slow step" in
          Eio.Time.sleep clock 0.25;
          Console.Display.succeed task);
      Format.pp_print_flush ppf ();
      Buffer.contents buf)

let ends_with_frozen_elapsed line =
  String.length line > 5 && String.ends_with ~suffix:"  0.0s" line

let test_frozen_clock_pins_every_elapsed () =
  let variable = "CONSOLE_FROZEN_CLOCK" in
  let running = capture_a_slow_task () in
  Alcotest.(check bool)
    "a real clock times the work" false
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

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "nested run reuses the existing display" `Quick
        test_nested_run_reuses_the_existing_display;
      Alcotest.test_case "run takes the caller's context" `Quick
        test_run_takes_the_callers_context;
      Alcotest.test_case "run refuses a formatter beside a context" `Quick
        test_run_refuses_a_formatter_beside_a_context;
      Alcotest.test_case "is_tty is false without a terminal" `Quick
        test_is_tty_is_false_without_a_terminal;
      Alcotest.test_case "dimensions are at least one cell" `Quick
        test_dimensions_are_at_least_one_cell;
      Alcotest.test_case "frozen clock pins every elapsed" `Quick
        test_frozen_clock_pins_every_elapsed;
    ] )
