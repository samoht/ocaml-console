(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Lwt.Syntax
open Display_harness
open Env_harness
module Task = Console.Task

let buffer () =
  let b = Buffer.create 256 in
  (b, Format.formatter_of_buffer b)

let shown b ppf =
  Format.pp_print_flush ppf ();
  Buffer.contents b

let lines s = List.filter (( <> ) "") (String.split_on_char '\n' s)

(* [with_reporter f] is [f seen], with a reporter installed around [f]. The
   reporter keeps each message from any domain or thread. [seen ()] is the
   messages so far, oldest first. *)
let with_reporter f =
  let seen = Atomic.make [] in
  let rec add s =
    let l = Atomic.get seen in
    if not (Atomic.compare_and_set seen l (s :: l)) then add s
  in
  let report _ _ ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun s ->
            add s;
            over ();
            k ())
          fmt)
  in
  let saved = Logs.reporter () in
  let mine = { Logs.report } in
  Logs.set_reporter mine;
  Fun.protect ~finally:(fun () -> Logs.set_reporter saved) @@ fun () ->
  let v = f (fun () -> List.rev (Atomic.get seen)) in
  Alcotest.(check bool)
    "the reporter is given back" true
    (Logs.reporter () == mine);
  v

let state =
  Alcotest.testable
    (fun ppf (s : Task.state) ->
      Fmt.string ppf
        (match s with
        | `Running -> "running"
        | `Succeeded -> "succeeded"
        | `Failed -> "failed"
        | `Cancelled -> "cancelled"))
    ( = )

let check_state msg expected t =
  Alcotest.check state msg expected (Task.state t)

let name = function Lwt.Canceled -> "Canceled" | e -> Printexc.to_string e

let outcome p =
  Lwt.catch
    (fun () -> Lwt.map (fun _ -> "resolved") p)
    (fun e -> Lwt.return (name e))

let is_pending p = match Lwt.state p with Lwt.Sleep -> true | _ -> false

(* Twelve jobs share the event loop, each in its own task. Half of them have a
   subtask per step. A job takes one step per turn of the loop and logs it, so
   the steps of all jobs interleave. Job 3 raises before its first wait, job 5
   is rejected half way, and job 7 is cancelled while it waits. *)
let jobs = 12
let steps i = 1 + (i mod 4)

let run_jobs () =
  let b, ppf = buffer () in
  let tasks = Array.make jobs None in
  let gate = fst (Lwt.task ()) in
  let job root i =
    Console_lwt.Task.run root (Fmt.str "job %02d" i) (fun t ->
        tasks.(i) <- Some t;
        if i = 3 then failwith "before any wait";
        Task.set_items t ~count:0 ~total:(steps i);
        let log r =
          Logs.app (fun m -> m "job %02d step %d" i r);
          Lwt.return_unit
        in
        let rec step r =
          if r = steps i then Lwt.return_unit
          else
            let* () = Lwt.pause () in
            if i = 5 && r = 1 then Lwt.fail_with "half way"
            else
              let* () = if i = 7 && r = 1 then gate else Lwt.return_unit in
              let* () =
                if i mod 2 = 0 then
                  Console_lwt.Task.run t (Fmt.str "step %d" r) (fun _ ->
                      let* () = Lwt.pause () in
                      log r)
                else log r
              in
              Task.advance t;
              step (r + 1)
        in
        step 0)
  in
  let root = ref None in
  let rejections =
    Lwt_main.run
      (Console_lwt.run ~ppf ~progress:`Plain (fun r ->
           root := Some r;
           let all = List.init jobs (fun i -> outcome (job r i)) in
           let* () = Lwt.pause () in
           Lwt.cancel gate;
           Lwt.all all))
  in
  let task i = Option.get tasks.(i) in
  (shown b ppf, rejections, Option.get !root, task)

let test_many_jobs () =
  with_reporter @@ fun seen ->
  let out, rejections, root, task = run_jobs () in
  Alcotest.(check (list string))
    "how each job ended"
    (List.init jobs (function
      | 3 -> "Failure(\"before any wait\")"
      | 5 -> "Failure(\"half way\")"
      | 7 -> "Canceled"
      | _ -> "resolved"))
    rejections;
  check_state "the root" `Succeeded root;
  for i = 0 to jobs - 1 do
    check_state (Fmt.str "job %d" i)
      (match i with 3 | 5 -> `Failed | 7 -> `Cancelled | _ -> `Succeeded)
      (task i)
  done;
  (* Each step is logged once, under its job, and under its subtask if it has
     one. *)
  let out_lines = lines out in
  let ran i r = not (i = 3 || (i = 5 && r >= 1) || (i = 7 && r >= 1)) in
  let where i r =
    if i mod 2 = 0 then Fmt.str "  job %02d \u{25B8} step %d: " i r
    else Fmt.str "  job %02d: " i
  in
  let logged = ref [] in
  for i = 0 to jobs - 1 do
    for r = 0 to steps i - 1 do
      let line = Fmt.str "%sjob %02d step %d" (where i r) i r in
      let count = List.length (List.filter (String.equal line) out_lines) in
      Alcotest.(check int) line (if ran i r then 1 else 0) count;
      if ran i r then logged := line :: !logged
    done
  done;
  (* The steps interleave. Among the jobs with subtasks, every first step
     comes before any second step. The same holds among the other jobs. A
     wait that blocked the loop would run each job to its end before the next
     one starts. *)
  let index line =
    let rec go n = function
      | [] -> max_int
      | l :: rest -> if String.equal l line then n else go (n + 1) rest
    in
    go 0 out_lines
  in
  let at parity r =
    List.filter_map
      (fun i ->
        if i mod 2 = parity && ran i r && r < steps i then
          Some (Fmt.kstr index "%sjob %02d step %d" (where i r) i r)
        else None)
      (List.init jobs Fun.id)
  in
  List.iter
    (fun parity ->
      let last_first = List.fold_left max min_int (at parity 0) in
      let first_second = List.fold_left min max_int (at parity 1) in
      Alcotest.(check bool)
        "the steps interleave" true
        (last_first < first_second))
    [ 0; 1 ];
  Alcotest.(check (list string)) "nothing reached the reporter" [] (seen ())

(* A task raises while other promises wait. The run is rejected with its
   exception once the display has closed, and the tasks that still wait end as
   cancelled. Waking them later changes nothing. *)
let test_raise_while_others_wait () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let waits = List.init 3 (fun _ -> Lwt.wait ()) in
  let sleepers = ref [] in
  let root = ref None in
  let run =
    Console_lwt.run ~ppf ~progress:`Tty (fun r ->
        root := Some r;
        let sleeper i (p, _) =
          Console_lwt.Task.run r (Fmt.str "sleeper %d" i) (fun t ->
              sleepers := t :: !sleepers;
              p)
        in
        let doomed =
          Console_lwt.Task.run r "doomed" (fun _ ->
              let* () = Lwt.pause () in
              Console_lwt.redraw ();
              failwith "boom")
        in
        Lwt.choose (doomed :: List.mapi sleeper waits))
  in
  let closed_when_rejected = ref None in
  Lwt.on_failure run (fun _ -> closed_when_rejected := Some (shown b ppf));
  Alcotest.(check string)
    "the exception" "Failure(\"boom\")"
    (Lwt_main.run (outcome run));
  let at_rejection = Option.get !closed_when_rejected in
  check_cursor_restored "by the rejection" at_rejection;
  check_state "the root" `Failed (Option.get !root);
  Alcotest.(check int) "sleepers" 3 (List.length !sleepers);
  List.iter (check_state "a sleeper" `Cancelled) !sleepers;
  List.iter (fun (_, u) -> Lwt.wakeup u ()) waits;
  List.iter (check_state "a sleeper woken" `Cancelled) !sleepers;
  Alcotest.(check string)
    "nothing written after the rejection" at_rejection (shown b ppf)

(* A redraw whose view raises cancels the program's promise. The run is rejected
   with the view's exception. *)
let test_view_raises_in_a_redraw () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let armed = ref false in
  let view _ =
    Console.View.fit (fun _ ->
        if !armed then failwith "view" else Console.View.text "rows")
  in
  let forever = fst (Lwt.task ()) in
  let work = ref None in
  let run =
    Console_lwt.run ~ppf ~progress:`Tty ~view (fun root ->
        Console_lwt.Task.run root "work" (fun t ->
            work := Some t;
            armed := true;
            forever))
  in
  Alcotest.(check string)
    "the view's exception" "Failure(\"view\")"
    (Lwt_main.run (outcome run));
  Alcotest.(check (option string))
    "the program's promise" (Some "Canceled")
    (match Lwt.state forever with Lwt.Fail e -> Some (name e) | _ -> None);
  check_state "the task" `Cancelled (Option.get !work);
  check_cursor_restored "the terminal" (shown b ppf)

(* A run whose view ends the run at its [n]th frame, by calling [wake] on the
   promise that the program returns. The redraws stop with the run: none of
   their timers is left. *)
let ended_by_view wake n =
  let _, ppf = buffer () in
  let read, seen = Lwt.wait () in
  let frames = ref 0 in
  let view _ =
    Console.View.fit @@ fun _ ->
    incr frames;
    if !frames = n && Lwt.is_sleeping read then wake seen ();
    Console.View.text "row"
  in
  let timers = Lwt_engine.timer_count () in
  Lwt_main.run (Console_lwt.run ~ppf ~progress:`Tty ~view (fun _ -> read));
  Alcotest.(check bool) "the frames before the end" true (!frames >= n);
  Alcotest.(check int) "no timer left" timers (Lwt_engine.timer_count ())

let test_redraws_stop_with_the_run () = ended_by_view Lwt.wakeup_later 3

(* [Lwt.wakeup] ends the run in the middle of a redraw. *)
let test_view_wakes_the_program () = ended_by_view Lwt.wakeup 3

(* [Lwt.cancel] on a running run reaches the promise that the program waits
   on, inside two tasks. The run is rejected only after the terminal is given
   back. *)
let test_cancel_a_run () =
  with_reporter @@ fun seen ->
  let b, ppf = buffer () in
  let tasks = ref [] in
  let root = ref None in
  let run =
    Console_lwt.run ~ppf ~progress:`Tty (fun r ->
        root := Some r;
        Console_lwt.Task.run r "outer" (fun outer ->
            tasks := outer :: !tasks;
            Console_lwt.Task.run outer "inner" (fun inner ->
                tasks := inner :: !tasks;
                Logs.app (fun m -> m "waiting");
                Lwt_unix.sleep 3600.)))
  in
  let at_rejection = ref "" in
  Lwt.on_failure run (fun _ -> at_rejection := shown b ppf);
  let drive =
    let* () = Lwt.pause () in
    Lwt.cancel run;
    outcome run
  in
  Alcotest.(check string) "rejected" "Canceled" (Lwt_main.run drive);
  check_cursor_restored "by the rejection" !at_rejection;
  check_state "the root" `Cancelled (Option.get !root);
  List.iter (check_state "a task" `Cancelled) !tasks;
  Alcotest.(check int) "two tasks" 2 (List.length !tasks);
  Alcotest.(check (list string)) "the log stayed in the run" [] (seen ())

(* A run inside a run gets the task that is current where it is called, after
   any number of waits. A run started outside joins it with the root. The first
   run's promise resolves only after the run that joined it ends. *)
let test_nested_and_joined_runs () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let events = ref [] in
  let event e = events := e :: !events in
  let gate_a, wake_a = Lwt.wait () and gate_b, wake_b = Lwt.wait () in
  let root_a = ref None and root_b = ref None in
  let program =
    let a =
      Console_lwt.run ~ppf ~progress:`Plain (fun root ->
          root_a := Some root;
          Console_lwt.Task.run root "a" (fun t ->
              let* () = Lwt.pause () in
              let* given = Console_lwt.run (fun given -> Lwt.return given) in
              Alcotest.(check bool) "nested" true (given == t);
              let* () =
                Console_lwt.Task.run t "deeper" (fun deeper ->
                    let* () = Lwt.pause () in
                    let* given =
                      Console_lwt.run ~label:"ignored" (fun g ->
                          let* () = Lwt.pause () in
                          let* g' = Console_lwt.run Lwt.return in
                          Alcotest.(check bool) "re-entrant" true (g == g');
                          Lwt.return g)
                    in
                    Alcotest.(check bool) "deeper" true (given == deeper);
                    Lwt.return_unit)
              in
              let* () = gate_a in
              event "a's function ends";
              Lwt.return_unit))
    in
    (* This run starts where no run is bound, so it joins the run with its
       root. The task that [a] holds while it waits stays [a]'s. *)
    let b' =
      Console_lwt.run (fun root ->
          root_b := Some root;
          let* () = gate_b in
          Console_lwt.Task.run root "b" (fun _ ->
              Logs.app (fun m -> m "from b");
              Lwt.return_unit))
    in
    Lwt.on_success a (fun () -> event "a's run ends");
    Lwt.on_success b' (fun () -> event "b's run ends");
    let* () = Lwt.pause () in
    Lwt.wakeup wake_a ();
    let* () = Lwt.pause () in
    let* () = Lwt.pause () in
    Alcotest.(check bool) "a waits for b" true (is_pending a);
    event "b is woken";
    Lwt.wakeup wake_b ();
    Lwt.join [ a; b' ]
  in
  Lwt_main.run program;
  Alcotest.(check bool)
    "one root" true
    (Option.get !root_a == Option.get !root_b);
  Alcotest.(check (list string))
    "the order"
    [ "a's function ends"; "b is woken"; "b's run ends"; "a's run ends" ]
    (List.rev !events);
  let out = lines (shown b ppf) in
  Alcotest.(check bool) "b's task" true (List.mem "  b: from b" out)

(* A task stays current in the Lwt thread while other threads log. A message
   from a thread of [Lwt_preemptive.detach] goes to the reporter that the run
   replaced. A message from another domain reaches the run outside any task.
   Neither one sees the task bound in Lwt's storage. *)
let test_threads_and_domains () =
  with_reporter @@ fun seen ->
  let b, ppf = buffer () in
  let busy = ref None in
  Lwt_main.run
    (Console_lwt.run ~ppf ~progress:`Plain (fun root ->
         Console_lwt.Task.run root "busy" (fun t ->
             busy := Some t;
             let finished = Atomic.make false in
             let thread =
               Lwt_preemptive.detach
                 (fun () ->
                   Logs.app (fun m -> m "from a thread");
                   Atomic.set finished true)
                 ()
             in
             (* The task stays current in this thread until the other thread has
                logged. *)
             while not (Atomic.get finished) do
               Thread.yield ()
             done;
             Domain.join
               (Domain.spawn (fun () ->
                    Logs.app (fun m -> m "from a domain");
                    Console_lwt.print (Console.View.text "printed by a domain")));
             let* () = thread in
             Logs.app (fun m -> m "from the task");
             Lwt.return_unit)));
  let out = lines (shown b ppf) in
  Alcotest.(check (list string))
    "the reporter the run replaced" [ "from a thread" ] (seen ());
  Alcotest.(check bool)
    "the task's own line" true
    (List.mem "  busy: from the task" out);
  Alcotest.(check bool)
    "the domain's line, outside the task" true
    (List.mem "from a domain" out);
  Alcotest.(check bool)
    "the domain's print" true
    (List.mem "printed by a domain" out);
  Alcotest.(check bool)
    "nothing from the thread" false
    (List.exists (contains "from a thread") out);
  let output = Task.output (Option.get !busy) in
  Alcotest.(check bool)
    "the task holds its line alone" false
    (List.exists (fun l -> contains "domain" l || contains "thread" l) output)

(* A promise that outlives its run keeps the run's bindings. If it is woken
   during the next run, it logs to that run, outside any task of the old run. If
   it is woken after every run, it logs to the reporter. A run whose
   [Lwt_main.run] returns while it left a promise sleeping lets the next run
   start. *)
let test_promises_outlive_their_run () =
  with_reporter @@ fun seen ->
  let b1, ppf1 = buffer () and b2, ppf2 = buffer () in
  let gate1, wake1 = Lwt.wait () and gate2, wake2 = Lwt.wait () in
  let late gate text =
    Lwt.async (fun () ->
        let* () = gate in
        Logs.app (fun m -> m "%s" text);
        Lwt.return_unit)
  in
  Lwt_main.run
    (Console_lwt.run ~ppf:ppf1 ~progress:`Plain (fun root ->
         Console_lwt.Task.run root "first" (fun _ ->
             late gate1 "late, in the second run";
             late gate2 "late, after every run";
             Lwt.async (fun () -> Lwt_unix.sleep 3600.);
             Lwt.return_unit)));
  Lwt_main.run
    (Console_lwt.run ~ppf:ppf2 ~progress:`Plain (fun root ->
         Console_lwt.Task.run root "second" (fun _ ->
             Lwt.wakeup wake1 ();
             Lwt.return_unit)));
  Lwt.wakeup wake2 ();
  let first = lines (shown b1 ppf1) and second = lines (shown b2 ppf2) in
  Alcotest.(check bool)
    "not in the first run" false
    (List.exists (contains "late") first);
  Alcotest.(check bool)
    "in the second run, outside its task" true
    (List.mem "late, in the second run" second);
  Alcotest.(check (list string))
    "after every run"
    [ "late, after every run" ]
    (seen ())

(* Signals that the program handles with [Lwt_unix.on_signal] stay its own,
   during a run and after it. Windows has no SIGTSTP, and its [Unix.kill] can
   only send SIGKILL. *)
let test_lwt_signal_handlers () =
  if Sys.win32 then Alcotest.skip ();
  let signals = [ Sys.sigint; Sys.sigterm; Sys.sigtstp ] in
  let caught = ref [] and waiting = ref None in
  let handler s =
    caught := s :: !caught;
    Option.iter (fun u -> Lwt.wakeup_later u ()) !waiting
  in
  let ids = List.map (fun s -> Lwt_unix.on_signal s handler) signals in
  let send s =
    let p, u = Lwt.wait () in
    waiting := Some u;
    Unix.kill (Unix.getpid ()) s;
    p
  in
  let send_all () = Lwt_list.iter_s send signals in
  let _, ppf = buffer () in
  Lwt_main.run (Console_lwt.run ~ppf ~progress:`Tty (fun _ -> send_all ()));
  Lwt_main.run (send_all ());
  List.iter Lwt_unix.disable_signal_handler ids;
  Alcotest.(check (list int))
    "each signal, during the run and after it" (signals @ signals)
    (List.rev !caught)

(* [Console_lwt.with_output] writes above the rows of the run. *)
let test_with_output () =
  let b, ppf = buffer () in
  Lwt_main.run
    (Console_lwt.run ~ppf ~progress:`Tty (fun root ->
         Console_lwt.Task.run root "work" (fun _ ->
             Console_lwt.with_output (fun o -> Fmt.pf o "above the rows@.");
             Lwt.return_unit)));
  Alcotest.(check bool)
    "the line" true
    (List.exists (contains "above the rows") (lines (shown b ppf)))

(* Ends the current recording, which closes its file. Windows cannot remove an
   open file. *)
let end_recording () =
  with_env
    [ ("CONSOLE_CAST", "") ]
    (fun () ->
      let unused = Format.formatter_of_buffer (Buffer.create 0) in
      ignore (Console__.Recording.start ~time:(fun () -> 0.) unused : bool))

(* A run recorded with [CONSOLE_CAST]: a header, then events that draw the task
   as its count changes, and that show the cursor again at the end. *)
let test_recording () =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () ->
      end_recording ();
      Sys.remove file)
  @@ fun () ->
  with_env
    [
      ("CONSOLE_CAST", file);
      ("COLUMNS", "30");
      ("LINES", "6");
      ("TERM", "xterm");
    ]
    (fun () ->
      let _, ppf = buffer () in
      Lwt_main.run
        (Console_lwt.run ~ppf (fun root ->
             Console_lwt.Task.run root "nap" (fun t ->
                 Task.set_items t ~count:1 ~total:2;
                 let* () = Lwt.pause () in
                 Task.set_items t ~count:2 ~total:2;
                 Lwt.return_unit))));
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  let data =
    List.map
      (fun (e : Cast_harness.event) -> e.data)
      (Cast_harness.events lines)
  in
  Alcotest.(check bool)
    "a header" true
    (contains "\"version\":2" (List.hd lines));
  Alcotest.(check bool)
    "the task's last count" true
    (List.exists (fun d -> contains "nap" d && contains "2/2" d) data);
  Alcotest.(check bool)
    "the cursor last" true
    (contains "\u{1B}[?25h" (List.nth data (List.length data - 1)))

let suite =
  ( "console_lwt",
    [
      Alcotest.test_case "many jobs" `Quick test_many_jobs;
      Alcotest.test_case "a raise while others wait" `Quick
        test_raise_while_others_wait;
      Alcotest.test_case "a view raises in a redraw" `Quick
        test_view_raises_in_a_redraw;
      Alcotest.test_case "redraws stop with the run" `Quick
        test_redraws_stop_with_the_run;
      Alcotest.test_case "a view wakes the program" `Quick
        test_view_wakes_the_program;
      Alcotest.test_case "cancel a run" `Quick test_cancel_a_run;
      Alcotest.test_case "nested and joined runs" `Quick
        test_nested_and_joined_runs;
      Alcotest.test_case "threads and domains" `Quick test_threads_and_domains;
      Alcotest.test_case "promises outlive their run" `Quick
        test_promises_outlive_their_run;
      Alcotest.test_case "signals handled with Lwt" `Quick
        test_lwt_signal_handlers;
      Alcotest.test_case "with_output" `Quick test_with_output;
      Alcotest.test_case "a recording" `Quick test_recording;
    ] )
