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

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "nested run reuses the existing display" `Quick
        test_nested_run_reuses_the_existing_display;
    ] )
