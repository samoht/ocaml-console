let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.clock env)

let test_run_returns_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      let result =
        Console_eio.run ~clock ~ppf (fun ctx ->
            let width, height = Console.Display.dimensions ctx in
            Alcotest.(check bool) "width" true (width > 10);
            Alcotest.(check bool) "height" true (height > 2);
            42)
      in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

let test_run_exception_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun _ctx -> failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

(* A CTRL-C in a cursor-hidden region must restore the cursor before the process
   dies, which the default SIGINT handler would otherwise skip. [on_interrupt]
   is the hook that guarantees it: a child installs the guard, SIGINTs itself,
   and must (a) run the cleanup and (b) still die from SIGINT -- not exit
   normally, which would mean the signal was swallowed. *)
let test_on_interrupt_runs_cleanup_then_dies_from_sigint () =
  let r, w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      Unix.close r;
      Console_eio.Terminal.on_interrupt
        (fun () -> ignore (Unix.write_substring w "C" 0 1))
        (fun () ->
          Unix.kill (Unix.getpid ()) Sys.sigint;
          (* Spin at allocating safe points until the runtime delivers the
             pending SIGINT to the handler above, which re-raises it. *)
          let rec spin () =
            ignore (Sys.opaque_identity (ref 0));
            spin ()
          in
          spin ())
  | pid ->
      Unix.close w;
      (* A SIGCHLD from the dying child can interrupt these blocking calls
         (an earlier Eio run leaves a handler installed without SA_RESTART), so
         retry on EINTR rather than mistaking it for a real failure. *)
      let rec read_eintr fd buf ofs len =
        try Unix.read fd buf ofs len
        with Unix.Unix_error (Unix.EINTR, _, _) -> read_eintr fd buf ofs len
      in
      let rec waitpid_eintr pid =
        try Unix.waitpid [] pid
        with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_eintr pid
      in
      let buf = Bytes.create 1 in
      let n = read_eintr r buf 0 1 in
      Unix.close r;
      let _, status = waitpid_eintr pid in
      Alcotest.(check int) "cleanup ran" 1 n;
      Alcotest.(check char) "cleanup wrote its byte" 'C' (Bytes.get buf 0);
      let died_from_sigint =
        match status with
        | Unix.WSIGNALED signal -> Int.equal signal Sys.sigint
        | WEXITED _ | WSTOPPED _ -> false
      in
      Alcotest.(check bool)
        "child died from re-raised SIGINT" true died_from_sigint

(* [on_interrupt] brackets the SIGINT handler: it installs its own for the
   duration of [f] and restores exactly the previous one afterwards. Compared
   against the nullary [Signal_ignore]/[Signal_default] constructors, which are
   safe to compare (unlike [Signal_handle], which carries a closure). *)
let test_on_interrupt_restores_previous_handler () =
  let original = Sys.signal Sys.sigint Sys.Signal_ignore in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigint original)
    (fun () ->
      Console_eio.Terminal.on_interrupt (fun () -> ()) (fun () -> ());
      let after = Sys.signal Sys.sigint Sys.Signal_ignore in
      Alcotest.(check bool)
        "previous handler restored" true
        (after = Sys.Signal_ignore))

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "on_interrupt runs cleanup then dies from SIGINT"
        `Quick test_on_interrupt_runs_cleanup_then_dies_from_sigint;
      Alcotest.test_case "on_interrupt restores previous handler" `Quick
        test_on_interrupt_restores_previous_handler;
    ] )
