(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness
open Env_harness

let with_clock f = Eio_main.run @@ fun _env -> f (Eio_mock.Clock.Mono.make ())

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
      Console.Task.run display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () -> Eio.Time.Mono.sleep clock 2.)
            (fun () -> Eio_mock.Clock.Mono.set_time clock (seconds 2.))));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let took s out = contains ("\u{2713} nap  " ^ s) out

(* A display reads the time on its monotonic clock. *)
let test_run_on_a_monotonic_clock () =
  Alcotest.(check bool) "the time it took" true (took "2.0s" (nap ()))

(* What a task prints that sleeps 1.5 and 0.5 seconds on [Console_eio.clock]
   of a mock clock. *)
let sleeps () =
  Eio_main.run @@ fun _ ->
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Task.run display "nap" (fun _ ->
          Eio.Time.Mono.sleep clock 1.5;
          Eio.Time.Mono.sleep clock 0.5));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* What a task prints whose two fibers sleep 1 and 2 seconds at once on
   [Console_eio.clock] of a mock clock. *)
let overlapping () =
  Eio_main.run @@ fun _ ->
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Task.run display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () -> Eio.Time.Mono.sleep clock 1.)
            (fun () -> Eio.Time.Mono.sleep clock 2.)));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* CONSOLE_DETERMINISTIC_CLOCK, set and not empty, makes the display read
   the time of the sleeps on a clock of [Console_eio.clock], and zero on
   another clock. *)
let test_deterministic_clock () =
  let deterministic f =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] (fun () -> f ())
  in
  Alcotest.(check bool) "a moving clock" true (took "2.0s" (nap ()));
  Alcotest.(check bool)
    "another clock stops" true
    (took "0.0s" (deterministic nap));
  let first = deterministic sleeps in
  Alcotest.(check bool) "one sleep after the other" true (took "2.0s" first);
  Alcotest.(check string) "the same twice" first (deterministic sleeps);
  Alcotest.(check bool)
    "sleeps at once overlap" true
    (took "2.0s" (deterministic overlapping));
  let empty =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "") ] (fun () -> nap ())
  in
  Alcotest.(check bool) "an empty value" true (took "2.0s" empty)

(* What a task prints that sleeps 1.5 then 0.5 seconds on [Console_eio.clock]
   of a mock clock that never moves. It fails if a sleep waits on that
   clock. *)
let still_nap () =
  Eio_main.run @@ fun _ ->
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let buf, ppf = buffer () in
  Eio.Fiber.first
    (fun () ->
      Console_eio.run ~clock ~ppf (fun root ->
          Console.Task.run root "nap" (fun t ->
              Eio.Time.Mono.sleep clock 1.5;
              Console.Task.set_items t ~count:1 ~total:2;
              Eio.Time.Mono.sleep clock 0.5;
              Console.Task.set_items t ~count:2 ~total:2)))
    (fun () ->
      for _ = 1 to 100_000 do
        Eio.Fiber.yield ()
      done;
      Alcotest.fail "a sleep waited on the clock behind");
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Under CONSOLE_DETERMINISTIC_CLOCK alone, a sleep takes no real time. *)
let test_sleeps_take_no_time () =
  with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] @@ fun () ->
  Alcotest.(check bool)
    "the time of the sleeps" true
    (contains "\u{2713} nap  2/2  2.0s" (still_nap ()))

(* What a task prints that sleeps 0.5 seconds on [Console_eio.clock], then
   awaits a promise that another fiber resolves after [n] yields. *)
let awaits n =
  Eio_main.run @@ fun _ ->
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun root ->
      let ready, resolve = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          Console.Task.run root "wait" (fun _ ->
              Eio.Time.Mono.sleep clock 0.5;
              Eio.Promise.await ready))
        (fun () ->
          for _ = 1 to n do
            Eio.Fiber.yield ()
          done;
          Eio.Promise.resolve resolve ()));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The redraws never move the time: a task that waits on another fiber
   shows the same time however many yields that fiber takes. *)
let test_redraws_keep_the_time () =
  with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] @@ fun () ->
  let few = awaits 10 in
  Alcotest.(check bool) "the time of the sleep" true (contains "wait  0.5s" few);
  Alcotest.(check string) "the same time" few (awaits 10_000)

(* The two builds of examples/columns, with sleeps on [Console_eio.clock].
   With a seed other than zero, each build works a random real time before
   each sleep, and another fiber yields a random number of times in a
   loop. *)
let perturbed ?cast ~progress seed =
  let env = match cast with Some f -> [ ("CONSOLE_CAST", f) ] | None -> [] in
  with_env env @@ fun () ->
  let rand = Random.State.make [| seed |] in
  let work () =
    if seed > 0 then
      for i = 1 to Random.State.int rand 100_000 do
        ignore (Sys.opaque_identity (i * i))
      done
  in
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let build root (name, pace, steps) =
    Console.Task.run root name (fun b ->
        List.iter
          (fun (step, total) ->
            Console.Task.run b step (fun task ->
                for count = 1 to total do
                  work ();
                  Eio.Time.Mono.sleep clock pace;
                  Console.Task.set_items task ~count ~total
                done))
          steps)
  in
  let builds =
    [
      ("linux", 0.08, [ ("fetch", 4); ("compile", 12); ("test", 8) ]);
      ( "macos",
        0.11,
        [ ("fetch", 4); ("compile", 12); ("test", 8); ("sign", 2) ] );
    ]
  in
  let buf, ppf = buffer () in
  let finished = ref false in
  Eio_main.run (fun _ ->
      Eio.Fiber.both
        (fun () ->
          Console_eio.run ~clock ~ppf ~progress ~keep_ended:true (fun root ->
              Eio.Fiber.List.iter (build root) builds);
          finished := true)
        (fun () ->
          while not !finished do
            for _ = 0 to Random.State.int rand 4 do
              Eio.Fiber.yield ()
            done
          done));
  Format.pp_print_flush ppf ();
  match cast with
  | Some file -> In_channel.with_open_bin file In_channel.input_all
  | None -> Buffer.contents buf

(* Whatever the real schedule, the virtual clock gives the same bytes: the
   same lines on a pipe, and the same frames on a terminal. *)
let test_deterministic_schedule () =
  with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] @@ fun () ->
  List.iter
    (fun (progress, ended) ->
      let calm = perturbed ~progress 0 in
      Alcotest.(check bool)
        "the builds take the longer time" true (contains ended calm);
      for seed = 1 to 50 do
        Alcotest.(check string)
          (Fmt.str "schedule %d" seed)
          calm (perturbed ~progress seed)
      done)
    [ (`Plain, "macos  2.9s"); (`Tty, "macos" ^ String.make 60 ' ' ^ "2.9s") ]

(* [recording f] is the lines of the recording that [CONSOLE_CAST] makes of
   [f], on a terminal of 30 columns by 6 rows. *)
let recording f =
  let file = Filename.temp_file "console" ".cast" in
  with_env
    [
      ("CONSOLE_DETERMINISTIC_CLOCK", "1");
      ("CONSOLE_CAST", file);
      ("COLUMNS", "30");
      ("LINES", "6");
      ("TERM", "xterm");
    ]
    f;
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  Sys.remove file;
  lines

let recorded_nap () = recording @@ fun () -> ignore (still_nap ())

(* asciicast v2: a header, then an event a line, each newline-delimited JSON:
   the rows drawn, and nothing on the output itself. *)
let test_cast_format () =
  match recorded_nap () with
  | [] -> Alcotest.fail "no header"
  | header :: lines ->
      Alcotest.check Cast_harness.json "the header"
        (Cast_harness.parse
           {|{"version":2,"width":30,"height":6,"env":{"TERM":"xterm"}}|})
        (Cast_harness.parse header);
      let events = List.map Cast_harness.event lines in
      Alcotest.(check bool) "events" true (List.length events > 2);
      List.iter
        (fun (e : Cast_harness.event) ->
          Alcotest.(check string) "an output event" "o" e.code)
        events;
      Alcotest.(check bool)
        "the rows drawn" true
        (fst
           (Console_vte.cursor_history (Cast_harness.event (List.hd lines)).data))

(* Each write is at its time on the virtual clock: the count when the first
   sleep ends, the last write when the second does. *)
let test_cast_times () =
  let events = Cast_harness.events (recorded_nap ()) in
  let times = List.map (fun (e : Cast_harness.event) -> e.time) events in
  Alcotest.(check bool) "in order" true (List.sort Float.compare times = times);
  Alcotest.(check bool)
    "at tenths of a second" true
    (List.for_all
       (fun t -> Float.abs ((t *. 10.) -. Float.round (t *. 10.)) < 1e-6)
       times);
  Alcotest.(check bool)
    "the count as the first sleep ends" true
    (List.exists
       (fun (e : Cast_harness.event) -> e.time = 1.5 && contains "1/2" e.data)
       events);
  Alcotest.(check (float 0.))
    "the end" 2.
    (List.nth times (List.length times - 1))

(* [recorded_text text] is the data of the events recorded for a run on
   [ppf], after which [text] is written on [ppf]. *)
let recorded_text text =
  let lines =
    recording @@ fun () ->
    Eio_main.run @@ fun _ ->
    let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
    let _, ppf = buffer () in
    Console_eio.run ~clock ~ppf (fun root ->
        Console.Task.run root "nap" (fun _ -> Eio.Time.Mono.sleep clock 0.1));
    Fmt.pf ppf "%s@?" text
  in
  String.concat ""
    (List.map
       (fun (e : Cast_harness.event) -> e.data)
       (Cast_harness.events lines))

(* A line feed is recorded after a carriage return, as a terminal's driver
   writes it, and one that has its carriage return already is left alone. *)
let test_cast_line_feeds () =
  let data = recorded_text "a\nb\r\nc\n" in
  Alcotest.(check bool)
    "each line feed after a carriage return" true
    (contains "a\r\nb\r\nc\r\n" data);
  Alcotest.(check bool) "no carriage return twice" false (contains "\r\r" data)

(* Whatever the real schedule, a recording is the same bytes. *)
let test_cast_schedule () =
  with_env
    [ ("CONSOLE_DETERMINISTIC_CLOCK", "1"); ("COLUMNS", "80"); ("LINES", "24") ]
  @@ fun () ->
  let recorded seed =
    let file = Filename.temp_file "console" ".cast" in
    Fun.protect
      ~finally:(fun () -> Sys.remove file)
      (fun () -> perturbed ~cast:file ~progress:`Tty seed)
  in
  let calm = recorded 0 in
  Alcotest.(check bool)
    "the builds take the longer time" true
    (List.exists
       (fun (e : Cast_harness.event) -> e.time = 2.86)
       (Cast_harness.events (String.split_on_char '\n' (String.trim calm))));
  for seed = 1 to 50 do
    Alcotest.(check string) (Fmt.str "schedule %d" seed) calm (recorded seed)
  done

(* A recording of four domains, each a loop over a sleep on
   [Console_eio.clock] and a tick of work. A domain counts its ticks on a
   task of its own and on a shared one. With a seed other than zero, each
   tick works a random real time, and another fiber yields a random number
   of times in a loop. *)
let recorded_domains seed =
  let rand = Random.State.make [| seed |] in
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_env
    [
      ("CONSOLE_CAST", file);
      ("COLUMNS", "40");
      ("LINES", "8");
      ("TERM", "xterm");
    ]
    (fun () ->
      Eio_main.run @@ fun env ->
      let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
      let domains = Console_eio.domain_mgr (Eio.Stdenv.domain_mgr env) in
      let _, ppf = buffer () in
      let ticks = 8 in
      let finished = ref false in
      Console_eio.run ~clock ~ppf ~progress:`Tty ~keep_ended:true (fun root ->
          let all = Console.Task.start root "all" in
          Console.Task.set_items all ~count:0 ~total:(4 * ticks);
          Eio.Fiber.both
            (fun () ->
              Eio.Fiber.List.iter
                (fun d ->
                  let task = Console.Task.start root (Fmt.str "domain %d" d) in
                  Console.Task.set_items task ~count:0 ~total:ticks;
                  Eio.Domain_manager.run domains (fun () ->
                      for i = 1 to ticks do
                        Eio.Time.Mono.sleep clock 0.1;
                        let rec work n s =
                          if n = 0 then s else work (n - 1) (Digest.string s)
                        in
                        let n =
                          if seed = 0 then 0 else Random.State.int rand 5
                        in
                        let s = work (1 + (n * 2_000)) "" in
                        Console.Task.log task
                          (Fmt.str "tick %d %d" i (String.length s));
                        Console.Task.advance task;
                        Console.Task.advance all
                      done;
                      Console.Task.succeed task))
                [ 0; 1; 2; 3 ];
              finished := true)
            (fun () ->
              while seed > 0 && not !finished do
                for _ = 0 to Random.State.int rand 4 do
                  Eio.Fiber.yield ()
                done
              done);
          Console.Task.succeed all));
  In_channel.with_open_bin file In_channel.input_all

(* Whatever the domains' real schedule, a recording is the same bytes. *)
let test_cast_domains () =
  let calm = recorded_domains 0 in
  Alcotest.(check bool) "every tick counted" true (contains "32/32" calm);
  for seed = 1 to 10 do
    Alcotest.(check string)
      (Fmt.str "schedule %d" seed)
      calm (recorded_domains seed)
  done

(* [Console_eio.domain_mgr] starts a domain, but in deterministic mode its
   new domain is the caller's. *)
let test_domain_mgr () =
  let elsewhere () =
    Eio_main.run @@ fun env ->
    let domains = Console_eio.domain_mgr (Eio.Stdenv.domain_mgr env) in
    let self = Domain.self () in
    Eio.Domain_manager.run domains (fun () -> Domain.self () <> self)
  in
  let mode value =
    with_env
      [ ("CONSOLE_CAST", ""); ("CONSOLE_DETERMINISTIC_CLOCK", value) ]
      elsewhere
  in
  Alcotest.(check bool) "another domain" true (mode "");
  Alcotest.(check bool) "the same domain" false (mode "1")

(* In deterministic mode, a clock of [Console_eio.clock] used from another
   domain raises. *)
let test_clock_owner () =
  with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] @@ fun () ->
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  match Domain.join (Domain.spawn (fun () -> Eio.Time.Mono.now clock)) with
  | _ -> Alcotest.fail "the clock was read from another domain"
  | exception Invalid_argument _ -> ()

let test_run_raises () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun display ->
              ignore (Console.Task.start display "doomed");
              failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the running task is cancelled" true
        (contains "\xe2\x97\x8b doomed" (Buffer.contents buf)))

(* An output that a cancelled fiber cannot write to, as an Eio flow. *)
let flow () =
  let buf = Buffer.create 256 in
  let out s o l =
    Eio.Fiber.check ();
    Buffer.add_substring buf s o l
  in
  (buf, Format.make_formatter out ignore)

(* A run that is cancelled still gives the terminal back. *)
let test_cancelled_run_restores () =
  with_clock (fun clock ->
      let buf, ppf = flow () in
      let started, start = Eio.Promise.create () in
      let result =
        Eio.Fiber.first
          (fun () ->
            Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
                ignore (Console.Task.start root "t");
                Eio.Promise.resolve start ();
                Eio.Fiber.await_cancel ()))
          (fun () ->
            Eio.Promise.await started;
            "cancelled")
      in
      Alcotest.(check string) "the winner" "cancelled" result;
      check_cursor_restored "cursor" (Buffer.contents buf))

(* A fiber cancelled while a view yields stops there, and its [Cancelled]
   does not end the run. *)
let test_view_cancelled () =
  with_clock (fun clock ->
      let _, ppf = buffer () in
      let armed = ref false in
      let view _ =
        Console.View.fit (fun _ ->
            if !armed then begin
              armed := false;
              Eio.Fiber.yield ()
            end;
            Console.View.text "row")
      in
      let value =
        Console_eio.run ~clock ~ppf ~progress:`Tty ~view (fun _ ->
            let winner =
              Eio.Fiber.first
                (fun () ->
                  armed := true;
                  Console_eio.print (Console.View.text "line");
                  Eio.Fiber.await_cancel ())
                (fun () -> "winner")
            in
            Alcotest.(check string) "the winner" "winner" winner;
            42)
      in
      Alcotest.(check int) "the run's value" 42 value)

(* A fiber cancelled inside [with_output] stays cancelled, and so does a task
   around it. *)
let test_with_output_cancelled () =
  with_clock (fun clock ->
      let _, ppf = flow () in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          let winner =
            Eio.Fiber.first
              (fun () ->
                Console_eio.with_output (fun _ -> Eio.Fiber.await_cancel ()))
              (fun () -> "winner")
          in
          Alcotest.(check string) "the winner" "winner" winner;
          let task = ref None in
          Eio.Fiber.first
            (fun () ->
              Console.Task.run root "inner" (fun t ->
                  task := Some t;
                  Console_eio.with_output (fun _ -> Eio.Fiber.await_cancel ())))
            ignore;
          Alcotest.(check bool)
            "the task is cancelled" true
            (Option.map Console.Task.state !task = Some `Cancelled)))

(* A write that a cancellation cuts half way, on an output that took that half,
   erases nothing above the region, and what it was to print still goes out. *)
let test_write_cut () =
  with_clock (fun clock ->
      let buf = Buffer.create 256 and entered, enter = Eio.Promise.create () in
      let slow = ref false in
      let out s o l =
        if !slow then begin
          slow := false;
          Buffer.add_substring buf s o (l / 2);
          Eio.Promise.resolve enter ();
          Eio.Fiber.await_cancel ()
        end
        else Buffer.add_substring buf s o l
      in
      let ppf = Format.make_formatter out ignore in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun _ ->
          List.iter
            (fun s -> Console_eio.print (Console.View.text s))
            [ "keep-1"; "keep-2"; "keep-3" ];
          slow := true;
          Eio.Fiber.first
            (fun () ->
              Console_eio.print (Console.View.text "PRINTED");
              Eio.Fiber.await_cancel ())
            (fun () -> Eio.Promise.await entered));
      let vte = Console_vte.v ~rows:20 ~cols:40 () in
      Console_vte.feed vte (Buffer.contents buf);
      List.iter
        (fun line ->
          Alcotest.(check bool)
            line true
            (List.exists (contains line) (Console_vte.screen vte)))
        [ "keep-1"; "keep-2"; "keep-3"; "PRINTED" ];
      let written = Buffer.contents buf in
      Alcotest.(check bool)
        "no update left open" false
        (Console_vte.synchronized_open written);
      check_cursor_restored "cursor" written)

(* A task ended by the cancellation of its fiber, on an output that a cancelled
   fiber cannot write to, leaves one row: the one it ends with. *)
let test_cancelled_task_row () =
  with_clock (fun clock ->
      let buf, ppf = flow () in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          let started, start = Eio.Promise.create () in
          Eio.Fiber.first
            (fun () ->
              Console.Task.run root "INNER" (fun _ ->
                  Console_eio.print (Console.View.text "above");
                  Eio.Promise.resolve start ();
                  Eio.Fiber.await_cancel ()))
            (fun () -> Eio.Promise.await started);
          Console_eio.print (Console.View.text "below"));
      let vte = Console_vte.v ~rows:12 ~cols:40 () in
      Console_vte.feed vte (Buffer.contents buf);
      let screen = List.filter (( <> ) "") (Console_vte.screen vte) in
      Alcotest.(check int)
        "one row of the task" 1
        (List.length (List.filter (contains "INNER") screen));
      Alcotest.(check bool)
        "the lines printed" true
        (List.mem "above" screen && List.mem "below" screen))

(* The redraw on the clock, under way as the run ends, goes out whole: the
   region the run leaves holds the task once. *)
let test_tick_as_the_run_ends () =
  with_clock (fun clock ->
      let buf = Buffer.create 256 and entered, enter = Eio.Promise.create () in
      let slow = ref false in
      let out s o l =
        if !slow then begin
          slow := false;
          Buffer.add_substring buf s o (l / 2);
          Eio.Promise.resolve enter ();
          Eio_unix.sleep 0.03;
          Buffer.add_substring buf s (o + (l / 2)) (l - (l / 2))
        end
        else Buffer.add_substring buf s o l
      in
      let ppf = Format.make_formatter out ignore in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          Console_eio.print (Console.View.text "above");
          Console.Task.run root "WORK" (fun _ ->
              slow := true;
              Eio_mock.Clock.Mono.advance clock;
              Eio.Promise.await entered));
      let vte = Console_vte.v ~rows:12 ~cols:40 () in
      Console_vte.feed vte (Buffer.contents buf);
      let screen = List.filter (( <> ) "") (Console_vte.screen vte) in
      Alcotest.(check (list string))
        "the screen"
        [ "above"; "\u{2713} WORK" ]
        (List.map (fun l -> String.sub l 0 (min (String.length l) 8)) screen))

(* A reader that pauses as a run ends loses nothing: a pager, say. *)
let test_reader_pauses () =
  with_clock (fun clock ->
      let buf = Buffer.create 256 and paused = ref false in
      let out s o l =
        if !paused then Eio_unix.sleep 1.2;
        Buffer.add_substring buf s o l
      in
      let ppf = Format.make_formatter out ignore in
      Console_eio.run ~clock ~ppf ~progress:`Plain (fun root ->
          ignore (Console.Task.start root "the last line");
          paused := true);
      Alcotest.(check bool)
        "the last line" true
        (contains "the last line" (Buffer.contents buf)))

(* An output that takes nothing once [work root stall] calls [stall] holds up
   the end of a cancelled run for a second at most, however many writes wait
   for it, and the next run starts. *)
let stalls work =
  with_clock (fun clock ->
      let stalled = ref false in
      let out _ _ _ = if !stalled then Eio.Fiber.await_cancel () in
      let ppf = Format.make_formatter out ignore in
      let stall () =
        stalled := true;
        Eio.Fiber.await_cancel ()
      in
      let start = Unix.gettimeofday () in
      let result =
        Eio.Fiber.first
          (fun () ->
            Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
                work root stall))
          (fun () ->
            Eio.Fiber.yield ();
            "cancelled")
      in
      let waited = Unix.gettimeofday () -. start in
      Alcotest.(check string) "the winner" "cancelled" result;
      if waited > 1.5 then Alcotest.failf "the run waited %.2fs" waited;
      Console_eio.run ~clock ~progress:`Plain ignore)

let test_output_stalls () =
  stalls (fun root stall ->
      ignore (Console.Task.start root "t");
      stall ())

let test_output_stalls_in_a_task () =
  stalls (fun root stall -> Console.Task.run root "t" (fun _ -> stall ()))

let test_output_stalls_in_nested_tasks () =
  stalls (fun root stall ->
      Console.Task.run root "a" (fun a ->
          Console.Task.run a "b" (fun b ->
              Console.Task.run b "c" (fun _ -> stall ()))))

(* A [Cancelled] raised by a view in a fiber that nothing cancelled is the
   view's own exception: the run raises it, and the print goes on. *)
let test_view_foreign_cancelled () =
  with_clock (fun clock ->
      let _, ppf = buffer () in
      let foreign = Eio.Cancel.Cancelled Not_found in
      let view _ = Console.View.fit (fun _ -> raise foreign) in
      let printed = ref false in
      Alcotest.check_raises "the run" foreign (fun () ->
          Console_eio.run ~clock ~ppf ~progress:`Tty ~view (fun _ ->
              Console_eio.print (Console.View.text "line");
              printed := true));
      Alcotest.(check bool) "the print went on" true !printed)

(* A report is not a cancellation point in the sense of [Eio.Cancel]: cleanup
   that reports or logs in a cancelled fiber runs to its end, and the fiber is
   still cancelled after. *)
let test_report_cancelled () =
  with_clock (fun clock ->
      let _, ppf = flow () in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          let task = Console.Task.start root "t" in
          let cleaned = ref false in
          let winner =
            Eio.Fiber.first
              (fun () ->
                Fun.protect
                  ~finally:(fun () ->
                    Console.Task.set_items task ~count:1 ~total:2;
                    Logs.app (fun m -> m "cleaning up");
                    Console.Task.log task "cleaned";
                    cleaned := true)
                  Eio.Fiber.await_cancel)
              (fun () -> "winner")
          in
          Alcotest.(check string) "the winner" "winner" winner;
          Alcotest.(check bool) "the cleanup ended" true !cleaned))

(* A [commit] that raises as the run ends, an error or an interrupt, makes the
   run raise it, once the cursor is shown again. *)
let test_commit_raises_at_end () =
  List.iter
    (fun e ->
      with_clock (fun clock ->
          let buf, ppf = buffer () in
          Alcotest.check_raises "the commit's" e (fun () ->
              Console_eio.run ~clock ~ppf ~progress:`Tty
                ~commit:(fun _ -> raise e)
                (fun root -> ignore (Console.Task.start root "running")));
          Format.pp_print_flush ppf ();
          check_cursor_restored "cursor" (Buffer.contents buf)))
    [ Failure "hook"; Sys.Break ]

(* An output that breaks, or an interrupt on the way out, does not stop the next
   run from starting. *)
let test_restore_raises () =
  let check_breaking e =
    with_clock (fun clock ->
        let saved = Format.pp_get_formatter_out_functions Fmt.stderr () in
        let failing = ref false in
        let out s o l = if !failing then raise e else saved.out_string s o l in
        Format.pp_set_formatter_out_functions Fmt.stderr
          { saved with out_string = out; out_flush = ignore };
        Fun.protect ~finally:(fun () ->
            Format.pp_set_formatter_out_functions Fmt.stderr saved)
        @@ fun () ->
        (try
           Console_eio.run ~clock ~progress:`Tty (fun _ ->
               Fmt.epr "an unended line";
               failing := true)
         with Sys.Break -> ());
        failing := false;
        let finished =
          Eio.Fiber.first
            (fun () ->
              Console_eio.run ~clock ~progress:`Tty ignore;
              true)
            (fun () ->
              for _ = 1 to 1000 do
                Eio.Fiber.yield ()
              done;
              false)
        in
        Alcotest.(check bool) "the next run ended" true finished)
  in
  check_breaking
    (Eio.Net.err
       (Eio.Net.Connection_reset (Eio_unix.Unix_error (Unix.EPIPE, "write", ""))));
  check_breaking Sys.Break

(* The run raises the program's exception whatever the last frame raises,
   unless the frame raises one the program must see, such as
   [Out_of_memory]. *)
let test_run_keeps_its_exception () =
  let last e _ =
    Console.View.fit (fun room ->
        match Console.Room.height room with
        | None -> raise e
        | Some _ -> Console.View.text "rows")
  in
  with_clock (fun clock ->
      let _, ppf = buffer () in
      Alcotest.check_raises "the program's" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf ~progress:`Plain ~label:"work"
            ~view:(last Exit) (fun _ -> failwith "boom"));
      Alcotest.check_raises "what the program must see" Out_of_memory (fun () ->
          Console_eio.run ~clock ~ppf ~progress:`Plain ~label:"work"
            ~view:(last Out_of_memory) (fun _ -> failwith "boom")))

(* The run raises the program's exception whatever the output raises on the
   way out. *)
let test_run_keeps_its_exception_on_output () =
  with_clock (fun clock ->
      let failing = ref false in
      let out _ _ _ = if !failing then raise Exit in
      let ppf = Format.make_formatter out ignore in
      Alcotest.check_raises "the program's" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf ~progress:`Plain ~label:"work" (fun _ ->
              failing := true;
              failwith "boom")))

(* A view that raises is dropped for the default one, and the run raises
   its exception: a view that raises at every frame, and one that raises
   only in the last, on a pipe and on a terminal. *)
let test_run_view_raises () =
  let always _ = Console.View.fit (fun _ -> failwith "view") in
  let last _ =
    Console.View.fit (fun room ->
        match Console.Room.height room with
        | None -> failwith "view"
        | Some _ -> Console.View.text "rows")
  in
  List.iter
    (fun (progress, view) ->
      with_clock (fun clock ->
          let buf, ppf = buffer () in
          Alcotest.check_raises "raised" (Failure "view") (fun () ->
              Console_eio.run ~clock ~ppf ~progress ~label:"work" ~view
                (fun root -> Console.Task.run root "step" ignore));
          Format.pp_print_flush ppf ();
          Alcotest.(check bool)
            "the default view" true
            (contains "work" (Buffer.contents buf))))
    [ (`Plain, always); (`Plain, last); (`Tty, always); (`Tty, last) ]

(* [commit] draws what a top task leaves in the scrollback, as with
   [Progress.show]. *)
let test_run_commit () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      let commit t =
        Some (Console.View.text ("left by " ^ Console.Task.label t))
      in
      Console_eio.run ~clock ~ppf ~progress:`Tty ~commit (fun root ->
          Console.Task.run root "step" ignore);
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the commit" true
        (contains "left by step" (Buffer.contents buf)))

(* [print] writes a view above the rows, styled, at the terminal's width,
   on a pipe in turn with the lines, and as events an output event of no
   task. *)
let test_run_print () =
  let printed progress =
    with_clock (fun clock ->
        let buf, ppf = buffer () in
        Fmt.set_style_renderer ppf `Ansi_tty;
        let warning =
          Console.View.(
            styled (Console.Style.fg Console.Color.red) (text "warning:"))
        in
        Console_eio.run ~clock ~ppf ~progress (fun root ->
            Console.Task.run root "step" (fun _ ->
                Console_eio.print
                  Console.View.(hcat ~gap:1 [ warning; text "careful" ])));
        Format.pp_print_flush ppf ();
        Buffer.contents buf)
  in
  let tty = printed `Tty in
  Alcotest.(check bool) "above the rows" true (contains "careful" tty);
  Alcotest.(check bool) "styled" true (contains "\027[31m" tty);
  Alcotest.(check bool) "on a pipe" true (contains "careful" (printed `Plain));
  Alcotest.(check bool)
    "an event" true
    (contains {|"event":"output","text":"warning: careful"|} (printed `Json))

(* [on_stderr f] is what [f] writes on standard error, read back from a
   file. *)
let on_stderr f =
  let file = Filename.temp_file "console" ".err" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Fun.protect
    ~finally:(fun () ->
      Format.pp_print_flush Fmt.stderr ();
      Unix.dup2 saved Unix.stderr;
      List.iter Unix.close [ saved; fd ])
    (fun () ->
      Unix.dup2 fd Unix.stderr;
      f ());
  In_channel.with_open_bin file In_channel.input_all

let test_print_outside_eio () =
  Alcotest.(check bool)
    "with_output" true
    (Console_eio.with_output (fun ppf -> ppf == Fmt.stderr));
  let printed =
    on_stderr (fun () -> Console_eio.print (Console.View.text "outside"))
  in
  Alcotest.(check string) "print" "outside\n" printed

let test_nested_run_reuses_the_display () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              Console.Task.succeed (Console.Task.start inner "nested work")));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses the outer output" true
        (String.starts_with
           ~prefix:"\xe2\x80\xa2 nested work\n\xe2\x9c\x93 nested work"
           (Buffer.contents buf)))

(* A run in another domain joins the one that runs: there is one terminal. *)
let test_run_in_another_domain_joins () =
  Eio_main.run @@ fun env ->
  let clock = Eio_mock.Clock.Mono.make () in
  let buf, ppf = buffer () in
  let inner =
    Console_eio.run ~clock ~ppf ~progress:`Plain (fun root ->
        let outer = Console.Task.start root "outer" in
        let inner, in_root =
          Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
              Console_eio.run ~clock ~ppf (fun given ->
                  Console.Task.run given "from a worker" (fun t ->
                      Console.Task.set_items t ~count:1 ~total:1;
                      (t, given == root))))
        in
        Alcotest.(check bool) "given the run's root" true in_root;
        Console.Task.succeed outer;
        inner)
  in
  Alcotest.(check bool) "succeeded" true (Console.Task.state inner = `Succeeded);
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "shown by the run" true
    (Astring.String.is_infix ~affix:"from a worker" (Buffer.contents buf));
  Console_eio.run ~clock ~ppf ignore

(* A worker of an Eio pool reports as a fiber of the run does. *)
let test_workers_report () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.mono_clock env in
  let pool =
    Eio.Executor_pool.create ~sw ~domain_count:3 (Eio.Stdenv.domain_mgr env)
  in
  let buf, ppf = buffer () in
  let steps = 10_000 in
  let count =
    Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
        Console.Task.run root "work" (fun t ->
            Console.Task.set_items t ~count:0 ~total:(4 * steps);
            Eio.Fiber.List.iter
              (fun d ->
                Eio.Executor_pool.submit_exn pool ~weight:1. (fun () ->
                    Console.Task.log t (Fmt.str "worker %d" d);
                    Fmt.kstr
                      (fun s -> Console_eio.print (Console.View.text s))
                      "above %d" d;
                    Console_eio.with_output (fun ppf ->
                        Fmt.pf ppf "output %d@." d);
                    Console_eio.redraw ();
                    for _ = 1 to steps do
                      Console.Task.advance t
                    done))
              [ 0; 1; 2; 3 ];
            Eio.Time.Mono.sleep clock 0.25;
            Console.Task.progress t))
  in
  Alcotest.(check bool) "every step" true (count = `Items (4 * steps, 4 * steps));
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  let has what d =
    Fmt.kstr
      (fun affix ->
        Alcotest.(check bool) affix true (Astring.String.is_infix ~affix out))
      "%s %d" what d
  in
  List.iter
    (fun d ->
      has "above" d;
      has "output" d)
    [ 0; 1; 2; 3 ]

(* Logs messages of another domain reach the display wherever they come from,
   and never wait for the domain that runs it: that one may be waiting for
   them. *)
let test_logs_from_other_domains () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf ~progress:`Plain (fun _ ->
      Domain.join
        (Domain.spawn (fun () -> Logs.app (fun m -> m "from a bare domain")));
      Domain.join
        (Domain.spawn (fun () ->
             Eio_main.run @@ fun _ ->
             for i = 1 to 100 do
               Logs.app (fun m -> m "from a worker %d" i)
             done)));
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  let has affix =
    Alcotest.(check bool) affix true (Astring.String.is_infix ~affix out)
  in
  has "from a bare domain";
  has "from a worker 1\n";
  has "from a worker 100\n"

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

let open_pty = Pty_harness.open_pty

(* [session how] is what [teardown.exe how] writes on a terminal, how it
   exits, and whether the terminal echoes typed input after it exits. *)
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
  let status = ref None and echo = ref None in
  let written =
    Pty_reader.read_all pty (fun () ->
        status := Some (Eio.Process.await child);
        echo := Some (Eio_unix.Fd.use_exn "tcgetattr" tty Unix.tcgetattr))
  in
  (Option.get !status, written, (Option.get !echo).Unix.c_echo)

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
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (how ^ ": standard output above the region, unlabelled")
        true
        (List.mem "a line of output" (Console_vte.screen ~scrollback:true vte));
      Alcotest.(check int)
        (how ^ ": the cursor is at the start of a line")
        0
        (snd (Console_vte.cursor_pos vte));
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

(* The final screen of what a session wrote. *)
let final_of out =
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Console_vte.screen ~scrollback:true vte

(* The final screen of [teardown.exe how]. *)
let final how = final_of (snd (teardown how))

(* Ctrl-C cancels the task, which prints its output under its line; a task
   that succeeds drops it. *)
let test_sigint_output () =
  let rows = final "sigint" in
  let rec under = function
    | l :: next :: _ when contains "\u{25cb} pushing" l -> next
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no cancelled line"
  in
  Alcotest.(check string)
    "the output stays" "  \u{203a} pushed 3 objects" (under rows);
  Alcotest.(check bool)
    "a success drops it" false
    (List.exists (contains "pushed 3 objects") (final "ok"))

(* What a task's function prints on the display's terminal, on either
   standard output, is its output, as are the Logs messages of the fibers it
   forks: it goes when the task succeeds and is printed under its line when
   it fails. A line printed within the root task goes above the rows. *)
let test_task_output () =
  let output =
    [
      "  \u{203a} an output line";
      "  \u{203a} a result line";
      "  \u{203a} a note line";
      "  ! a forked note";
    ]
  in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l ->
        List.filteri (fun i _ -> i < List.length output) rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let failed = final "logs-failed" in
  Alcotest.(check (list string)) "under the failure" output (under failed);
  let rows = final "logs" in
  List.iter
    (fun rows ->
      Alcotest.(check bool)
        "within the root, above the rows" true
        (List.mem "a root line" rows))
    [ failed; rows ];
  Alcotest.(check bool)
    "the success line" true
    (List.exists (contains "\u{2713} work") rows);
  List.iter
    (fun text ->
      Alcotest.(check bool)
        ("folded away: " ^ text) false
        (List.exists (contains text) rows))
    [ "an output line"; "a result line"; "a note line"; "a forked note" ]

(* What a thread prints on the display's terminal goes above the rows, as
   output outside any task. *)
let test_printed_elsewhere () =
  let status, out = teardown "elsewhere" in
  Alcotest.(check bool)
    "the program succeeds" true
    (match status with `Exited 0 -> true | _ -> false);
  Alcotest.(check bool)
    "the line shows" true
    (List.mem "from a thread" (final_of out))

(* A line written outside a task is not the output of the task in which
   the next line is written, even when it waited for a flush until then. *)
let test_held_line () =
  let rows = final "held" in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l -> rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let output = List.filter (contains "\u{203a}") (under rows) in
  Alcotest.(check (list string))
    "the task's line alone under it"
    [ "  \u{203a} a task line" ]
    output;
  Alcotest.(check bool)
    "the held line above the rows" true
    (List.exists (String.starts_with ~prefix:"a held line") rows)

(* The first domain the program starts during a run leaves what it prints
   to the display, and once the run ends standard error writes as Format's
   functions do once a domain started. *)
let test_spawn () =
  let rows = final "spawn" in
  List.iter
    (fun i ->
      let line = Fmt.str "spawned line %d" i in
      Alcotest.(check bool)
        ("a row of its own: " ^ line)
        true (List.mem line rows))
    [ 1; 2; 3 ];
  let rec under = function
    | l :: next :: _ when contains "\u{2717} work" l -> next
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  Alcotest.(check string)
    "the task's output" "  \u{203a} a task line" (under rows);
  Alcotest.(check bool)
    "after the run, Format's functions" true
    (List.mem "held until a flush: true" rows)

let test_sigint_teardown () =
  let _, out = teardown "sigint" in
  Alcotest.(check bool)
    "SIGINT interrupted a live display" true (contains hide out);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "SIGINT restores the cursor" true
    (Console_vte.cursor_visible vte)

let signaled_by_sigint = function
  | `Signaled s -> Int.equal s Sys.sigint
  | _ -> false

(* SIGINT in the middle of a frame's write ends the program once that
   frame is written: the screen is the one SIGINT leaves between frames. *)
let test_sigint_mid_write () =
  let status, out = teardown "sigint-mid-write" in
  Alcotest.(check bool)
    "SIGINT ends the program" true
    (signaled_by_sigint status);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "the cursor is shown" true
    (Console_vte.cursor_visible vte);
  Alcotest.(check (list string))
    "a clean final frame" (final "sigint") (final_of out)

(* A write that never ends does not hold SIGINT back for ever. *)
let test_sigint_blocked () =
  let status, _ = teardown "sigint-blocked" in
  Alcotest.(check bool)
    "SIGINT ends the program" true
    (signaled_by_sigint status)

(* However a session ends, text written after it is drawn as on a fresh
   terminal: at the first column, in the terminal's own colours. *)
let test_usable_after () =
  List.iter
    (fun (how, no_color) ->
      let _, out, _ = session ~no_color how in
      let name = Fmt.str "%s%s" how (if no_color then " (NO_COLOR)" else "") in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (name ^ ": cursor shown") true
        (Console_vte.cursor_visible vte);
      let y, x = Console_vte.cursor_pos vte in
      Alcotest.(check int) (name ^ ": first column") 0 x;
      Console_vte.feed vte "AFTER";
      let plain = Console_vte.v ~rows:1 ~cols:80 () in
      Console_vte.feed plain "AFTER";
      let g = Console_vte.grid vte and reference = Console_vte.grid plain in
      let i = Grid.idx g ~x:0 ~y and j = Grid.idx reference ~x:0 ~y:0 in
      let colours g i =
        List.concat_map
          (fun c ->
            let r, g, b, a = Ansi.Color.to_rgba c in
            [ r; g; b; a ])
          [ Console_vte.fg g i; Console_vte.bg g i ]
      in
      Alcotest.(check (list int))
        (name ^ ": default colours")
        (colours reference j) (colours g i))
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
      View.pp ppf
        (View.styled
           Style.(bold + fg (Theme.succeeded Theme.nord))
           (View.text "x")));
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    String.sub s 0 (String.index s 'x')
  in
  Alcotest.(check bool) "colour, in the theme" true (contains nord out)

(* [waitpid flags pid] is the status of [pid], waited for again when a
   signal interrupts the wait. *)
let rec waitpid flags pid =
  try snd (Unix.waitpid flags pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

(* [stopped_by_sigtstp pid] is [true] iff [pid] stops on SIGTSTP. A process
   that never stopped is gone by the time waitpid looks. *)
let stopped_by_sigtstp pid =
  match
    Eio_unix.run_in_systhread ~label:"waitpid" (fun () ->
        waitpid [ Unix.WUNTRACED ] pid)
  with
  | Unix.WSTOPPED s -> Int.equal s Sys.sigtstp
  | _ | (exception Unix.Unix_error (Unix.ECHILD, _, _)) -> false

(* Ctrl-Z gives the terminal back before the program stops: the region is
   erased and the cursor shown. SIGTSTP's default action is to stop the
   process: in a group of its own, under a parent in the same session, the
   group is not orphaned and the stop takes effect, which waitpid sees before
   anything continues it. Once continued, the region is drawn again and the
   work goes on. *)
let test_suspend () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw ~pgid:0
      (Eio.Stdenv.process_mgr env)
      ~env:[| "TERM=xterm-256color"; "LANG=en_US.UTF-8" |]
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:"./teardown.exe"
      [ "teardown.exe"; "suspend" ]
  in
  let pid = Eio.Process.pid child in
  let stopped = ref false and status = ref None in
  let out =
    Pty_reader.read_all pty (fun () ->
        stopped := stopped_by_sigtstp pid;
        if !stopped then Unix.kill pid Sys.sigcont;
        status := Some (Eio.Process.await child))
  in
  Alcotest.(check bool) "stopped by SIGTSTP" true !stopped;
  let show = "\027[?25h" in
  let hidden = Option.get (Astring.String.find_sub ~sub:hide out) in
  match Astring.String.find_sub ~start:hidden ~sub:show out with
  | None -> Alcotest.fail "the cursor is not shown before the stop"
  | Some stop ->
      let before = String.sub out 0 (stop + String.length show) in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte before;
      Alcotest.(check bool)
        "the cursor is shown" true
        (Console_vte.cursor_visible vte);
      Alcotest.(check bool)
        "the region is erased" false
        (List.exists (contains "pausing") (Console_vte.screen vte));
      Alcotest.(check bool)
        "the region is drawn again, the cursor hidden" true
        (Option.is_some (Astring.String.find_sub ~start:stop ~sub:hide out));
      Alcotest.(check bool)
        "the work goes on" true
        (List.exists (contains "\u{2713} pausing") (final_of out));
      Alcotest.(check bool)
        "exit status" true
        (match !status with Some (`Exited 0) -> true | _ -> false)

(* A stop during a write that does not end within a second gives the
   terminal up, and the program goes on once continued. *)
let test_suspend_blocked () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Pty_harness.open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw ~pgid:0
      (Eio.Stdenv.process_mgr env)
      ~env:[| "TERM=xterm-256color"; "LANG=en_US.UTF-8" |]
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:"./teardown.exe"
      [ "teardown.exe"; "stop-blocked" ]
  in
  let pid = Eio.Process.pid child in
  let stopped = ref false and status = ref None in
  let out =
    Pty_reader.read_all pty (fun () ->
        stopped := stopped_by_sigtstp pid;
        if !stopped then Unix.kill pid Sys.sigcont;
        status := Some (Eio.Process.await child))
  in
  Alcotest.(check bool) "stopped by SIGTSTP" true !stopped;
  Alcotest.(check bool)
    "the escape code cut is ended" true
    (contains "\027[?2\024\027\\" out);
  Alcotest.(check bool)
    "ends once continued" true
    (match !status with Some (`Exited 0) -> true | _ -> false);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "the cursor is shown" true
    (Console_vte.cursor_visible vte)

(* In a process group that no shell controls, Linux and macOS discard a
   stop: the terminal is given back and the region drawn again once, and the
   work goes on. FreeBSD stops the process all the same, so there the test
   continues it. A display that gave the terminal back again and again would
   show the cursor without end; the reader stops it then. *)
let test_suspend_without_stop () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let freebsd =
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.line [ "uname"; "-s" ]
    = "FreeBSD"
  in
  let pty = open_pty ~sw in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:[| "TERM=xterm-256color"; "LANG=en_US.UTF-8" |]
      ~fds:
        (List.map (fun fd -> (fd, Eio_unix.Pty.tty pty, `Blocking)) [ 0; 1; 2 ])
      ~executable:"./teardown.exe"
      [ "teardown.exe"; "orphaned-suspend" ]
  in
  let out = Buffer.create 4096 and buf = Cstruct.create 4096 in
  let shown = ref 0 and spun = ref false in
  let rec read () =
    let n = Eio.Flow.single_read (Eio_unix.Pty.source pty) buf in
    let s = Cstruct.to_string ~len:n buf in
    Buffer.add_string out s;
    shown := !shown + List.length (Astring.String.cuts ~sep:"\027[?25h" s) - 1;
    if !shown > 50 && not !spun then begin
      spun := true;
      Unix.kill (Eio.Process.pid child) Sys.sigkill
    end;
    read ()
  in
  let status =
    Eio.Fiber.first
      (fun () ->
        let pid = Eio.Process.pid child in
        if freebsd && stopped_by_sigtstp pid then Unix.kill pid Sys.sigcont;
        Eio.Process.await child)
      (fun () ->
        try read () with End_of_file | Eio.Io _ -> Eio.Fiber.await_cancel ())
  in
  Alcotest.(check bool)
    (Fmt.str "the region is given back a bounded number of times (%d)" !shown)
    false !spun;
  Alcotest.(check bool)
    "the work goes on" true
    (match status with `Exited 0 -> true | _ -> false)

(* Output outside a task that the program never flushes is not held
   without bound: all but the last 64 KiB of it is on the terminal before
   the program goes on. *)
let test_unflushed_output_is_bounded () =
  let _, out, _ = session "unflushed" in
  match Astring.String.find_sub ~sub:"MARK" out with
  | None -> Alcotest.fail "no mark"
  | Some k ->
      let after = String.length out - k in
      let held =
        List.length (Astring.String.cuts ~sep:"line " (String.sub out k after))
        - 1
      in
      Alcotest.(check bool)
        (Fmt.str "at most 64 KiB held back (%d lines)" held)
        true
        (held * String.length "line 000000\n" <= 65536)

let test_sigint_secret () =
  let status, _, echo = session "secret" in
  Alcotest.(check bool)
    "SIGINT still ends the program" true
    (match status with `Signaled s -> Int.equal s Sys.sigint | _ -> false);
  Alcotest.(check bool) "SIGINT during a password restores echo" true echo

let exited n = function `Exited m -> Int.equal n m | _ -> false

(* A signal handled in another domain, as when the display's domain sleeps
   or its thread blocks the signal, still ends the program by that signal;
   [exit] from another domain or thread while the display redraws ends it
   with its status. Either way the cursor is shown. *)
let test_elsewhere_ends () =
  List.iter
    (fun (how, ends) ->
      let status, out = teardown how in
      Alcotest.(check bool) (how ^ ": how the program ends") true (ends status);
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (how ^ ": the cursor is shown")
        true
        (Console_vte.cursor_visible vte))
    [
      ("sigint-beside", signaled_by_sigint);
      ("sigint-masked", signaled_by_sigint);
      ("exit-domain-drawing", exited 3);
      ("exit-thread-drawing", exited 4);
    ]

(* A restoration that fails still lets the signal end the program. *)
let test_sigint_restore_fails () =
  let status, _ = teardown "secret-lost" in
  Alcotest.(check bool)
    "SIGINT still ends the program" true
    (signaled_by_sigint status)

(* A write that never ends gives way to a sequence written once on the
   terminal: whatever escape code it cut is ended, and what is written next
   shows as on a fresh terminal, the cursor shown. *)
let test_last_resort () =
  List.iter
    (fun (how, ends) ->
      let status, out = teardown how in
      Alcotest.(check bool) (how ^ ": how the program ends") true (ends status);
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (how ^ ": the cursor is shown")
        true
        (Console_vte.cursor_visible vte);
      Alcotest.(check bool)
        (how ^ ": no update left open")
        false
        (Console_vte.synchronized_open out);
      Console_vte.feed vte "AFTER";
      Alcotest.(check bool)
        (how ^ ": what comes next shows")
        true
        (List.exists (contains "AFTER") (Console_vte.screen vte)))
    [
      ("cut-link", signaled_by_sigint);
      ("cut-sequence", signaled_by_sigint);
      ("exit-thread-blocked", exited 3);
      ("exit-domain-blocked", exited 3);
    ]

let test_quiet_terminal_capture () =
  let status, out = teardown "quiet-ok" in
  Alcotest.(check bool)
    "the program succeeds" true
    (match status with `Exited 0 -> true | _ -> false);
  Alcotest.(check (list string))
    "successful task output is hidden; root output stays" [ "root line" ]
    (final_of out)

let test_cast_without_deterministic () =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_env [ ("CONSOLE_CAST", file); ("CONSOLE_DETERMINISTIC_CLOCK", "") ]
  @@ fun () ->
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let _, ppf = buffer () in
  let slept = ref false in
  Eio.Fiber.first
    (fun () ->
      Console_eio.run ~clock ~ppf (fun _ ->
          Eio.Time.Mono.sleep clock 1.;
          slept := true))
    (fun () ->
      for _ = 1 to 1000 do
        Eio.Fiber.yield ()
      done);
  Alcotest.(check bool)
    "recording sleeps without advancing the real clock" true !slept

(* [recorded f] is the lines of the recording of a run that calls [f] under
   [with_output]. *)
let recorded f =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_env [ ("CONSOLE_CAST", file); ("CONSOLE_DETERMINISTIC_CLOCK", "1") ]
  @@ fun () ->
  Eio_main.run (fun _ ->
      let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
      let _, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun _ -> Console_eio.with_output f));
  In_channel.with_open_bin file In_channel.input_lines

(* asciicast v2, "o": data must be valid UTF-8. *)
let test_cast_utf_8 () =
  let lines = recorded (fun ppf -> Fmt.pf ppf "bad \xff byte\n%!") in
  Alcotest.(check bool)
    "the bad byte replaced" true
    (List.exists
       (fun (e : Cast_harness.event) -> contains "bad \u{FFFD} byte" e.data)
       (Cast_harness.events lines))

(* [piped.exe args] under [Unix.waitpid], with its output dropped. *)
let piped_status args =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null) @@ fun () ->
  let pid =
    Unix.create_process "./piped.exe"
      (Array.of_list ("piped.exe" :: args))
      null null null
  in
  waitpid [] pid

(* The default action of SIGINT and SIGTERM is abnormal termination (XBD
   signal.h): the process ends by the signal, not by an exit that a shell would
   also show as 128 plus its number. *)
let test_signal_terminates () =
  let check msg signo args =
    Alcotest.(check bool)
      msg true
      (match piped_status args with
      | Unix.WSIGNALED s -> Int.equal s signo
      | _ -> false)
  in
  check "SIGINT after a view raised" Sys.sigint [ "signal" ];
  check "SIGTERM after a view raised" Sys.sigterm [ "sigterm" ];
  let file = Filename.temp_file ~temp_dir:"." "console" ".out" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  List.iter
    (fun (signo, mode) ->
      check mode signo [ mode; file ];
      check_cursor_restored mode
        (In_channel.with_open_bin file In_channel.input_all))
    [ (Sys.sigint, "idle-signal"); (Sys.sigterm, "idle-sigterm") ]

let test_repeated_signal_mid_write () =
  let input =
    Unix.open_process_args_in "./teardown.exe"
      [| "teardown.exe"; "repeated-signal"; "int"; "int"; "0"; "false" |]
  in
  let raw = In_channel.input_all input in
  let status = Unix.close_process_in input in
  Alcotest.(check bool)
    "the signal still ends the process" true
    (status = Unix.WSIGNALED Sys.sigint);
  let terminal = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed terminal raw;
  Alcotest.(check bool)
    "a second SIGINT waits until the terminal is restored" true
    (Console_vte.cursor_visible terminal);
  Alcotest.(check bool)
    "the final synchronized update is complete" true
    (String.ends_with ~suffix:"\027[?2026l" raw)

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "two SIGINTs during a write restore the terminal"
        `Quick test_repeated_signal_mid_write;
      Alcotest.test_case "recording alone makes sleeps virtual" `Quick
        test_cast_without_deterministic;
      Alcotest.test_case "a signal ends the process by that signal" `Quick
        test_signal_terminates;
      Alcotest.test_case "SIGTSTP during a write that does not end" `Quick
        test_suspend_blocked;
      Alcotest.test_case "a recording is valid UTF-8" `Quick test_cast_utf_8;
      Alcotest.test_case "quiet hides successful terminal output" `Quick
        test_quiet_terminal_capture;
      Alcotest.test_case "a cancelled run restores the terminal" `Quick
        test_cancelled_run_restores;
      Alcotest.test_case "cancelled in a view" `Quick test_view_cancelled;
      Alcotest.test_case "cancelled around with_output" `Quick
        test_with_output_cancelled;
      Alcotest.test_case "a write that is cut" `Quick test_write_cut;
      Alcotest.test_case "a task ended in a cancelled fiber" `Quick
        test_cancelled_task_row;
      Alcotest.test_case "a redraw under way as the run ends" `Quick
        test_tick_as_the_run_ends;
      Alcotest.test_case "a reader that pauses" `Quick test_reader_pauses;
      Alcotest.test_case "an output that stalls" `Quick test_output_stalls;
      Alcotest.test_case "an output that stalls in a task" `Quick
        test_output_stalls_in_a_task;
      Alcotest.test_case "an output that stalls in nested tasks" `Quick
        test_output_stalls_in_nested_tasks;
      Alcotest.test_case "a foreign Cancelled" `Quick
        test_view_foreign_cancelled;
      Alcotest.test_case "a report in a cancelled fiber" `Quick
        test_report_cancelled;
      Alcotest.test_case "a commit raises as the run ends" `Quick
        test_commit_raises_at_end;
      Alcotest.test_case "the output breaks as the run ends" `Quick
        test_restore_raises;
      Alcotest.test_case "the program's exception goes on" `Quick
        test_run_keeps_its_exception;
      Alcotest.test_case "the program's exception goes on past the output"
        `Quick test_run_keeps_its_exception_on_output;
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
      Alcotest.test_case "unflushed output is held back boundedly" `Quick
        test_unflushed_output_is_bounded;
      Alcotest.test_case "SIGINT restores the terminal cursor" `Quick
        test_sigint_teardown;
      Alcotest.test_case "SIGINT in the middle of a write" `Quick
        test_sigint_mid_write;
      Alcotest.test_case "SIGINT ends a write that never ends" `Quick
        test_sigint_blocked;
      Alcotest.test_case "a write that never ends, then the last resort" `Quick
        test_last_resort;
      Alcotest.test_case "SIGINT when a restoration fails" `Quick
        test_sigint_restore_fails;
      Alcotest.test_case "ended from another domain or thread" `Quick
        test_elsewhere_ends;
      Alcotest.test_case "a stop in a group no shell controls resumes once"
        `Quick test_suspend_without_stop;
      Alcotest.test_case "Ctrl-Z gives the terminal back, fg redraws" `Quick
        test_suspend;
      Alcotest.test_case "SIGINT prints a cancelled task's output" `Quick
        test_sigint_output;
      Alcotest.test_case "a task's output on a terminal" `Quick test_task_output;
      Alcotest.test_case "a held line stays outside the next task" `Quick
        test_held_line;
      Alcotest.test_case "printed from a thread" `Quick test_printed_elsewhere;
      Alcotest.test_case "a domain started during a run" `Quick test_spawn;
      Alcotest.test_case "run returns" `Quick test_run_returns;
      Alcotest.test_case "run on a monotonic clock" `Quick
        test_run_on_a_monotonic_clock;
      Alcotest.test_case "deterministic clock" `Quick test_deterministic_clock;
      Alcotest.test_case "sleeps take no real time" `Quick
        test_sleeps_take_no_time;
      Alcotest.test_case "redraws keep the time" `Quick
        test_redraws_keep_the_time;
      Alcotest.test_case "a deterministic clock ignores the schedule" `Quick
        test_deterministic_schedule;
      Alcotest.test_case "a recording's header and events" `Quick
        test_cast_format;
      Alcotest.test_case "a recording's times are virtual" `Quick
        test_cast_times;
      Alcotest.test_case "a recording ignores the schedule" `Quick
        test_cast_schedule;
      Alcotest.test_case "a recording's line feeds" `Quick test_cast_line_feeds;
      Alcotest.test_case "a recording of domains ignores the schedule" `Quick
        test_cast_domains;
      Alcotest.test_case "domain_mgr" `Quick test_domain_mgr;
      Alcotest.test_case "a virtual clock belongs to its domain" `Quick
        test_clock_owner;
      Alcotest.test_case "run raises" `Quick test_run_raises;
      Alcotest.test_case "a view that raises" `Quick test_run_view_raises;
      Alcotest.test_case "a commit" `Quick test_run_commit;
      Alcotest.test_case "print" `Quick test_run_print;
      Alcotest.test_case "print outside Eio" `Quick test_print_outside_eio;
      Alcotest.test_case "nested run reuses the display" `Quick
        test_nested_run_reuses_the_display;
      Alcotest.test_case "a run in another domain joins" `Quick
        test_run_in_another_domain_joins;
      Alcotest.test_case "workers report" `Quick test_workers_report;
      Alcotest.test_case "logs from other domains" `Quick
        test_logs_from_other_domains;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "teardown on a terminal" `Quick test_teardown;
    ] )
