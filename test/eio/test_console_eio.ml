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

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "nested run reuses the existing display" `Quick
        test_nested_run_reuses_the_existing_display;
      Alcotest.test_case "is_tty is false without a terminal" `Quick
        test_is_tty_is_false_without_a_terminal;
      Alcotest.test_case "dimensions are at least one cell" `Quick
        test_dimensions_are_at_least_one_cell;
    ] )
