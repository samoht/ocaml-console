(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Lwt.Syntax
open Display_harness
module Task = Console.Task

(* A clock that reads zero and never ticks on its own. The display redraws
   only when the program asks, so what it writes is the same on each run. *)
let frozen =
  Console_lwt.Clock.v ~now:(fun () -> 0.) ~sleep:(fun _ -> fst (Lwt.task ()))

let buffer () =
  let b = Buffer.create 256 in
  (b, Format.formatter_of_buffer b)

let shown b ppf =
  Format.pp_print_flush ppf ();
  Buffer.contents b

let lines s = List.filter (( <> ) "") (String.split_on_char '\n' s)

(* [with_reporter f] is [f seen], with a reporter installed around [f] that
   keeps each message from any domain or thread. [seen ()] are the messages
   so far, oldest first. *)
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

(* Twelve jobs share the event loop, each in a task of its own and half of
   them with a subtask per step. A job takes a step per turn of the loop and
   logs it, so the steps of all jobs interleave. Job 3 raises before its
   first wait, job 5 is rejected half way, and job 7 is cancelled while it
   waits. *)
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
      (Console_lwt.run ~clock:frozen ~ppf ~progress:`Plain (fun r ->
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
  (* Each step is logged once, under its job, and its subtask if any. *)
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
     comes before any second step, and the same holds among the others. A
     wait that blocked the loop would run each job to its end before the next
     starts. *)
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
          Some (index (Fmt.str "%sjob %02d step %d" (where i r) i r))
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
  Alcotest.(check (list string)) "nothing reached the reporter" [] (seen ());
  (* The same program writes the same bytes. *)
  let again, _, _, _ = run_jobs () in
  Alcotest.(check string) "the same transcript" out again

(* A task raises while other promises wait. The run is rejected with its
   exception once the display has closed, and the tasks that still wait end
   cancelled. Waking them later changes nothing. *)
let test_raise_while_others_wait () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let waits = List.init 3 (fun _ -> Lwt.wait ()) in
  let sleepers = ref [] in
  let root = ref None in
  let run =
    Console_lwt.run ~clock:frozen ~ppf ~progress:`Tty (fun r ->
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

(* A redraw on the clock whose view raises cancels the program's promise, and
   the run is rejected with the view's exception. The clock's sleep does not
   take [Lwt.cancel], so the redraws must stop by themselves once the run
   ends. *)
let test_view_raises_in_a_redraw () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let ticks = Queue.create () in
  let sleep _ =
    let p, u = Lwt.wait () in
    Queue.push u ticks;
    p
  in
  let clock = Console_lwt.Clock.v ~now:(fun () -> 0.) ~sleep in
  let armed = ref false in
  let view _ =
    Console.View.fit (fun _ ->
        if !armed then failwith "view" else Console.View.text "rows")
  in
  let forever = fst (Lwt.task ()) in
  let work = ref None in
  let run =
    Console_lwt.run ~clock ~ppf ~progress:`Tty ~view (fun root ->
        Console_lwt.Task.run root "work" (fun t ->
            work := Some t;
            forever))
  in
  let drive =
    let* () = Lwt.pause () in
    armed := true;
    Lwt.wakeup (Queue.pop ticks) ();
    outcome run
  in
  Alcotest.(check string)
    "the view's exception" "Failure(\"view\")" (Lwt_main.run drive);
  Alcotest.(check (option string))
    "the program's promise" (Some "Canceled")
    (match Lwt.state forever with Lwt.Fail e -> Some (name e) | _ -> None);
  check_state "the task" `Cancelled (Option.get !work);
  let written = shown b ppf in
  check_cursor_restored "the terminal" written;
  armed := false;
  Queue.iter (fun u -> Lwt.wakeup u ()) ticks;
  Alcotest.(check string) "no redraw after the end" written (shown b ppf)

(* The redraws stop with the run, even on a clock whose sleep does not take
   [Lwt.cancel]. A tick that ends after the run draws nothing and waits for
   no other. *)
let test_redraws_stop_with_the_run () =
  let b, ppf = buffer () in
  let ticks = Queue.create () in
  let sleep _ =
    let p, u = Lwt.wait () in
    Queue.push u ticks;
    p
  in
  let clock = Console_lwt.Clock.v ~now:(fun () -> 0.) ~sleep in
  let frames = ref 0 in
  let view _ =
    Console.View.fit (fun _ ->
        incr frames;
        Console.View.text "rows")
  in
  Lwt_main.run
    (Console_lwt.run ~clock ~ppf ~progress:`Tty ~view (fun root ->
         Console_lwt.Task.run root "work" (fun _ ->
             let* () = Lwt.pause () in
             Lwt.wakeup (Queue.pop ticks) ();
             Lwt.pause ())));
  let written = shown b ppf and drawn = !frames in
  Alcotest.(check int) "one wait on the clock" 1 (Queue.length ticks);
  Lwt.wakeup (Queue.pop ticks) ();
  Alcotest.(check int) "no frame after the end" drawn !frames;
  Alcotest.(check string) "no write after the end" written (shown b ppf);
  Alcotest.(check int) "no wait after the end" 0 (Queue.length ticks)

(* [Lwt.cancel] on a running run reaches the promise the program waits on,
   inside two tasks, and the run is rejected only once the terminal is given
   back. *)
let test_cancel_a_run () =
  with_reporter @@ fun seen ->
  let b, ppf = buffer () in
  let tasks = ref [] in
  let root = ref None in
  let run =
    Console_lwt.run ~clock:frozen ~ppf ~progress:`Tty (fun r ->
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

(* Runs inside a run are given the task current where they are called, after
   any number of waits. A run started outside it joins it with the root. The
   first run's promise resolves only once the run that joined it ends. *)
let test_nested_and_joined_runs () =
  with_reporter @@ fun _ ->
  let b, ppf = buffer () in
  let events = ref [] in
  let event e = events := e :: !events in
  let gate_a, wake_a = Lwt.wait () and gate_b, wake_b = Lwt.wait () in
  let root_a = ref None and root_b = ref None in
  let program =
    let a =
      Console_lwt.run ~clock:frozen ~ppf ~progress:`Plain (fun root ->
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
    (* Started where no run is bound, it joins the run with its root. The
       task that [a] holds while it waits stays [a]'s. *)
    let b' =
      Console_lwt.run ~clock:frozen (fun root ->
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

(* A task stays current in the thread of Lwt while other threads log. A
   message from a thread of [Lwt_preemptive.detach] goes to the reporter the
   run replaced. One from another domain reaches the run outside any task.
   Neither sees the task bound in Lwt's storage. *)
let test_threads_and_domains () =
  with_reporter @@ fun seen ->
  let b, ppf = buffer () in
  let busy = ref None in
  Lwt_main.run
    (Console_lwt.run ~clock:frozen ~ppf ~progress:`Plain (fun root ->
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
             (* The task stays current in this thread until the other one
                has logged. *)
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

(* A promise that outlives its run keeps the run's bindings. Woken during the
   next run, what it logs goes to that run, outside any task of the dead one.
   Woken after every run, it logs to the reporter. A run whose [Lwt_main.run] returns
   while it left a promise sleeping lets the next one start. *)
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
    (Console_lwt.run ~clock:frozen ~ppf:ppf1 ~progress:`Plain (fun root ->
         Console_lwt.Task.run root "first" (fun _ ->
             late gate1 "late, in the second run";
             late gate2 "late, after every run";
             Lwt.async (fun () -> Lwt_unix.sleep 3600.);
             Lwt.return_unit)));
  Lwt_main.run
    (Console_lwt.run ~clock:frozen ~ppf:ppf2 ~progress:`Plain (fun root ->
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

let suite =
  ( "console-lwt",
    [
      Alcotest.test_case "many jobs" `Quick test_many_jobs;
      Alcotest.test_case "a raise while others wait" `Quick
        test_raise_while_others_wait;
      Alcotest.test_case "a view raises in a redraw" `Quick
        test_view_raises_in_a_redraw;
      Alcotest.test_case "redraws stop with the run" `Quick
        test_redraws_stop_with_the_run;
      Alcotest.test_case "cancel a run" `Quick test_cancel_a_run;
      Alcotest.test_case "nested and joined runs" `Quick
        test_nested_and_joined_runs;
      Alcotest.test_case "threads and domains" `Quick test_threads_and_domains;
      Alcotest.test_case "promises outlive their run" `Quick
        test_promises_outlive_their_run;
    ] )
