(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

(* Console_eio driver, on a buffer: no terminal, so each task line, log line
   and the result is printed as plain text. *)

module D = Console.Display

(* Run [f] under a fresh display captured into a string, then return the
   text. *)
let capture ?header f =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Console_eio.run ~clock ~ppf ?header f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let mem ~msg needle haystack =
  Alcotest.(check bool) msg true (contains needle haystack)

(* A finished task commits its label; set_result `Ok prints the verdict. *)
let test_run_banner () =
  let out =
    capture ~header:"Build" (fun t ->
        let step = D.task t "compile foo.ml" in
        D.succeed step;
        D.set_result t ~detail:"3 files, 0 errors" (`Ok "build succeeded"))
  in
  mem ~msg:"task label" "compile foo.ml" out;
  mem ~msg:"detail" "3 files, 0 errors" out;
  mem ~msg:"verdict marker" "✓" out;
  mem ~msg:"verdict message" "build succeeded" out

(* set_result `Fail prints the failure verdict. *)
let test_run_fail () =
  let out =
    capture (fun t ->
        let step = D.task t "link" in
        D.fail step;
        D.set_result t (`Fail "link failed"))
  in
  mem ~msg:"fail marker" "✗" out;
  mem ~msg:"fail message" "link failed" out

(* An unhandled exception clears the region and propagates. *)
let test_run_raises () =
  Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
      ignore
        (capture (fun t ->
             let _ = D.task t "doomed" in
             failwith "boom")))

(* Logs messages are routed into the same permanent semantic history while the
   driver owns the reporter. *)
let named = Logs.Src.create "console.test"

let test_reporter () =
  let out =
    capture (fun t ->
        Logs.set_level (Some Logs.Debug);
        let step = D.task t "step" in
        Logs.app (fun m -> m "application-line");
        Logs.app (fun m -> m "╭───╮\n│ x │\n╰───╯");
        Logs.warn (fun m -> m "warning-line");
        Logs.info ~src:named (fun m -> m "named-line");
        D.succeed step)
  in
  mem ~msg:"a named source says its name" "[console.test] named-line" out;
  mem ~msg:"application log" "application-line" out;
  mem ~msg:"multiline log keeps physical rows" "╭───╮\n│ x │\n╰───╯\n" out;
  mem ~msg:"warning log" "warning-line" out

let suite =
  ( "display",
    [
      ("run banner", `Quick, test_run_banner);
      ("run fail", `Quick, test_run_fail);
      ("run raises", `Quick, test_run_raises);
      ("reporter", `Quick, test_reporter);
    ] )

module Report_domains = struct
  type sink = {
    ppf : Format.formatter;
    buffer : Buffer.t;
    foreign : int Atomic.t;
  }

  let sink ?(on_write = fun () -> ()) () =
    let owner = Domain.self () in
    let foreign = Atomic.make 0 in
    let buffer = Buffer.create 128 in
    let ppf =
      Format.make_formatter
        (fun text offset length ->
          if not (Int.equal (Domain.self () :> int) (owner :> int)) then
            Atomic.incr foreign;
          on_write ();
          Buffer.add_substring buffer text offset length)
        (fun () -> ())
    in
    { ppf; buffer; foreign }

  let with_runtime f =
    Eio_main.run @@ fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () -> f env)

  let run env sink f =
    Console_eio.run ~clock:(Eio.Stdenv.clock env) ~ppf:sink.ppf f

  let pool env sw =
    Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env)

  let source = Logs.Src.create "console.worker"

  let send ?(over = fun () -> ()) ?(continued = fun () -> ()) reporter text =
    reporter.Logs.report source Logs.Info ~over continued (fun m -> m "%s" text)

  let acknowledged sink over_count continued reporter text expected =
    send reporter text
      ~over:(fun () -> Atomic.incr over_count)
      ~continued:(fun () ->
        Alcotest.(check string)
          "continuation follows complete output" expected
          (Buffer.contents sink.buffer);
        Atomic.incr continued)

  let restored previous =
    Alcotest.(check bool)
      "teardown restores the previous reporter" true
      (Logs.reporter () == previous)

  let owner_domain () =
    with_runtime @@ fun env ->
    let sink = sink ~on_write:Eio.Fiber.yield () in
    let previous = Logs.reporter () in
    let over_count = Atomic.make 0 and continued = Atomic.make 0 in
    run env sink (fun _ ->
        Eio.Switch.run @@ fun sw ->
        Eio.Executor_pool.submit_exn (pool env sw) ~weight:1. (fun () ->
            let reporter = Logs.reporter () in
            acknowledged sink over_count continued reporter "worker-first"
              "[console.worker] worker-first\n";
            acknowledged sink over_count continued reporter "worker-second"
              "[console.worker] worker-first\n[console.worker] worker-second\n"));
    Format.pp_print_flush sink.ppf ();
    Alcotest.(check int)
      "zero foreign accesses to the owned formatter" 0 (Atomic.get sink.foreign);
    Alcotest.(check string)
      "both complete records"
      "[console.worker] worker-first\n[console.worker] worker-second\n"
      (Buffer.contents sink.buffer);
    Alcotest.(check int) "each log completed once" 2 (Atomic.get over_count);
    Alcotest.(check int)
      "each continuation acknowledged once" 2 (Atomic.get continued);
    restored previous

  let output_error () =
    with_runtime @@ fun env ->
    let first = ref true in
    let sink =
      sink
        ~on_write:(fun () ->
          if !first then begin
            first := false;
            failwith "owner sink refused"
          end)
        ()
    in
    let previous = Logs.reporter () in
    let over_count = Atomic.make 0 and continued = Atomic.make 0 in
    run env sink (fun _ ->
        Eio.Switch.run @@ fun sw ->
        Alcotest.check_raises "actual output refusal reaches the worker"
          (Failure "owner sink refused") (fun () ->
            Eio.Executor_pool.submit_exn (pool env sw) ~weight:1. (fun () ->
                send (Logs.reporter ()) "refused"
                  ~over:(fun () -> Atomic.incr over_count)
                  ~continued:(fun () -> Atomic.incr continued))));
    Alcotest.(check int)
      "failed output has zero completion acknowledgements" 0
      (Atomic.get over_count);
    Alcotest.(check int)
      "failed output has zero continuations" 0 (Atomic.get continued);
    Alcotest.(check int)
      "zero foreign sink accesses" 0 (Atomic.get sink.foreign);
    restored previous

  let stale_reporter () =
    with_runtime @@ fun env ->
    let sink = sink () in
    let previous = Logs.reporter () in
    let reporter = run env sink (fun _ -> Logs.reporter ()) in
    Alcotest.check_raises "captured reporter names its closed scope"
      (Invalid_argument "Console_eio: the display reporter is closed")
      (fun () -> send reporter "late");
    Alcotest.(check string)
      "closed reporter writes zero bytes" ""
      (Buffer.contents sink.buffer);
    restored previous

  let blocked_sink enter =
    let in_output = ref false in
    let output =
      sink
        ~on_write:(fun () ->
          in_output := true;
          Fun.protect
            (fun () ->
              ignore (Eio.Promise.try_resolve enter () : bool);
              Eio.Fiber.await_cancel ())
            ~finally:(fun () -> in_output := false))
        ()
    in
    (output, in_output)

  let close_waiting_worker ?(fail = true) () =
    with_runtime @@ fun env ->
    Eio.Switch.run @@ fun outer ->
    let workers = pool env outer in
    let entered, enter = Eio.Promise.create () in
    let sink, in_output = blocked_sink enter in
    let previous = Logs.reporter () in
    let worker = ref None and continued = Atomic.make 0 in
    let run () =
      run env sink (fun _ ->
          worker :=
            Some
              (Eio.Executor_pool.submit_fork ~sw:outer workers ~weight:1.
                 (fun () ->
                   send (Logs.reporter ()) "waiting" ~continued:(fun () ->
                       Atomic.incr continued)));
          Eio.Promise.await entered;
          if fail then failwith "close display")
    in
    if fail then
      Alcotest.check_raises "original body failure survives teardown"
        (Failure "close display") run
    else run ();

    (* Each of the worker's writes can see the closed reporter, so on a host
       with spare cores it may report the same error twice. *)
    let closed =
      Invalid_argument "Console_eio: the display reporter is closed"
    in
    (match Eio.Promise.await_exn (Option.get !worker) with
    | () -> Alcotest.fail "external worker is released by closure: no error"
    | exception e when e = closed -> ()
    | exception Eio.Exn.Multiple exns
      when List.for_all (fun (e, _) -> e = closed) exns ->
        ());
    Alcotest.(check bool)
      "consumer stopped before display teardown" false !in_output;
    Alcotest.(check int)
      "unfinished log has zero continuations" 0 (Atomic.get continued);
    restored previous

  let suite =
    ( "report_domains",
      [
        Alcotest.test_case "worker logs retain formatter ownership" `Quick
          owner_domain;
        Alcotest.test_case "worker receives actual output failure" `Quick
          output_error;
        Alcotest.test_case "captured reporter refuses after teardown" `Quick
          stale_reporter;
        Alcotest.test_case "scope closure releases a waiting external worker"
          `Quick
          (close_waiting_worker ~fail:true);
        Alcotest.test_case "normal closure releases a waiting external worker"
          `Quick
          (close_waiting_worker ~fail:false);
      ] )
end

let report_domains_suite = Report_domains.suite
