(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [teardown.exe how] ends inside a live display in the way [how] says. With no
   argument, its task succeeds. With [sigint], [exit], [raise] or [cancel], it
   is interrupted, calls [exit] (as a CLI's error path does), raises to the
   toplevel, or has its fiber cancelled. In these four cases it also prints a
   line on standard output, which goes above the region, and its task prints a
   line of output. The other arguments are:
   - [secret]: SIGINT arrives while it reads a password. With [secret-lost],
     standard input has also lost its terminal.
   - [colourless]: it turns colour off before the display.
   - [widgets]: it prints every widget before the display.
   - [themed]: it gives standard error a theme and makes no other choice.
   - [logs] or [logs-failed]: its task prints and logs, then succeeds or fails.
   - [quiet-ok], [quiet-fail] or [quiet-cancel]: in quiet mode, its task prints
     and logs, then succeeds, fails or is cancelled.
   - [suspend]: SIGTSTP stops it, as Ctrl-Z does, and it resumes when continued.
     With [orphaned-suspend], it runs in its own session, where Linux and macOS
     discard the stop.
   - [unflushed]: it prints lines that it never flushes.
   - [held]: it prints a line outside a task without flushing it, then a line in
     a task that fails.
   - [spawn]: it starts its first domain during the display.
   - [elsewhere]: a thread prints on its terminal.
   - [sigint-mid-write]: SIGINT arrives in the middle of writing a frame. With
     [sigint-blocked], that write then never ends.
   - [cut-link] or [cut-sequence]: SIGINT arrives during a write that never
     ends, cut inside a link or inside a control sequence.
   - [exit-thread-blocked] or [exit-domain-blocked]: a thread or a domain calls
     [exit] during such a write.
   - [stop-blocked]: SIGTSTP stops it during a write that ends after a while.
   - [sigint-beside]: SIGINT arrives while another domain computes. With
     [sigint-masked], the display's thread also blocks SIGINT.
   - [exit-domain-drawing] or [exit-thread-drawing]: a domain or a thread calls
     [exit] while the display redraws.
   - [repeated-signal first second cut closing]: two signals arrive in a row in
     the middle of a write, cut after [cut] bytes. [first] and [second] are each
     [int] or [term]. *)

(* SIGINT arrives once the password is masked. *)
let secret env =
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  Eio.Fiber.both
    (fun () ->
      (* SIGINT ends the program before the answer comes. *)
      match Console_eio.Prompt.secret prompt "password: " with
      | Ok _ | Error _ -> ())
    (fun () ->
      while (Unix.tcgetattr Unix.stdin).Unix.c_echo do
        Eio.Fiber.yield ()
      done;
      Unix.kill (Unix.getpid ()) Sys.sigint)

(* As [secret], but standard input no longer has the terminal, so restoring the
   echo fails. *)
let secret_lost env =
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  Eio.Fiber.both
    (fun () ->
      (* SIGINT ends the program before the answer comes. *)
      match Console_eio.Prompt.secret prompt "password: " with
      | Ok _ | Error _ -> ())
    (fun () ->
      while (Unix.tcgetattr Unix.stdin).Unix.c_echo do
        Eio.Fiber.yield ()
      done;
      let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
      Unix.dup2 null Unix.stdin;
      Unix.kill (Unix.getpid ()) Sys.sigint);
  Fmt.pr "still running@."

(* Before the display, the program turns colour off itself, as [--color=never]
   does. *)
let colourless _env =
  Fmt.set_style_renderer Fmt.stdout `None;
  Fmt.set_style_renderer Fmt.stderr `None;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.succeed (Console.Task.start root "plain"))

(* [setup] reads the locale, then every widget is printed on standard output,
   then a display with a bar runs. *)
let widgets _env =
  let open Console in
  Console_tty.setup ();
  let long = View.text (String.concat " " (List.init 20 (Fun.const "word"))) in
  let leaf = Tree.v long [] in
  Fmt.pr "%a@.%a@.%a@.%a@.%a@." View.pp
    (Table.view
       [
         Table.column (View.text "a");
         Table.column ~overflow:`Truncate (View.text "b");
       ]
       [ [ View.text "1"; long ] ])
    View.pp
    (Panel.view ~title:long long)
    View.pp
    (Tree.view (Tree.v long [ Tree.v long [ leaf ]; leaf ]))
    View.pp (Rule.view ~label:long ()) View.pp
    (Bar.horizontal ~brackets:true ~width:10 0.55);
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Task.start root "counting" in
      Task.set_items task ~count:1 ~total:3;
      Console_eio.with_output ignore;
      Task.succeed task)

(* Before the display, the program sets a theme and makes no colour choice. *)
let themed _env =
  Console.Theme.set Fmt.stderr Console.Theme.nord;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.succeed (Console.Task.start root "themed"))

(* A task prints its output, a line on each standard output, and a line within
   the root task. A fiber that the task forks logs a Logs message. With [fails],
   the task fails. *)
let logs ~fails _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.run root "work" (fun task ->
          Console.Task.log task "an output line";
          Fmt.pr "a result line@.";
          Fmt.epr "a note line@.";
          Console.Task.within root (fun () -> Fmt.pr "a root line@.");
          Eio.Fiber.both
            (fun () -> Logs.warn (fun m -> m "a forked note"))
            Eio.Fiber.yield;
          if fails then Console.Task.fail task))

let suspend _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Console.Task.start root "pausing" in
      Console_eio.with_output ignore;
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      Console.Task.set_items task ~count:1 ~total:2;
      Console_eio.with_output ignore;
      Console.Task.succeed task)

(* SIGINT arrives in the middle of writing a frame, when half of it is on the
   terminal. With [blocked], the write then never ends. *)
let mid_write ~blocked _env =
  let armed = ref false in
  let out = Format.pp_get_formatter_out_functions Fmt.stderr () in
  let out_string s o n =
    if !armed && n > 1 then begin
      armed := false;
      out.out_string s o (n / 2);
      out.out_flush ();
      Unix.kill (Unix.getpid ()) Sys.sigint;
      if blocked then begin
        let r, _ = Unix.pipe () in
        ignore (Unix.read r (Bytes.create 1) 0 1 : int)
      end;
      out.out_string s (o + (n / 2)) (n - (n / 2))
    end
    else out.out_string s o n
  in
  Format.pp_set_formatter_out_functions Fmt.stderr { out with out_string };
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Console.Task.start root "pushing" in
      Console.Task.log task "pushed 3 objects";
      Fmt.pr "a line of output@.";
      Console_eio.with_output (fun _ -> armed := true);
      Console.Task.succeed task)

(* A frame's write is cut at [cut s] bytes of the frame [s], then [k ()] runs.
   The rest of the write never comes, or comes after [block ()]. *)
let never () =
  let r, _ = Unix.pipe () in
  ignore (Unix.read r (Bytes.create 1) 0 1 : int)

let blocked_at ?(block = never) ~cut k _env =
  let armed = ref false in
  let out = Format.pp_get_formatter_out_functions Fmt.stderr () in
  let out_string s o n =
    if !armed && n > 1 then begin
      armed := false;
      out.out_string s o (cut (String.sub s o n));
      out.out_flush ();
      k ();
      block ();
      out.out_string s (o + cut (String.sub s o n)) (n - cut (String.sub s o n))
    end
    else out.out_string s o n
  in
  Format.pp_set_formatter_out_functions Fmt.stderr { out with out_string };
  let view _ =
    Console.View.link ~uri:"https://example.org/" (Console.View.text "a link")
  in
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~view (fun root ->
      ignore (Console.Task.start root "pushing");
      Console_eio.with_output (fun _ -> armed := true);
      Console_eio.redraw ())

(* Cut inside the link's address, or inside the first control sequence. *)
let in_link s =
  let rec find i =
    if String.sub s i 6 = "\027]8;;h" then i + 6 else find (i + 1)
  in
  find 0

let in_sequence _ = 4
let interrupt () = Unix.kill (Unix.getpid ()) Sys.sigint

let exit_from how () =
  let leave () = exit 3 in
  match how with
  | `Thread -> ignore (Thread.create leave () : Thread.t)
  | `Domain -> ignore (Domain.spawn leave : unit Domain.t)

let repeated_signal ~first ~second ~cut ~closing _env =
  let armed = ref false in
  let out_string s offset length =
    if !armed && length > 1 then begin
      armed := false;
      let cut = 1 + (cut mod (length - 1)) in
      output_substring stdout s offset cut;
      flush stdout;
      Unix.kill (Unix.getpid ()) first;
      Gc.minor ();
      Unix.kill (Unix.getpid ()) second;
      Gc.minor ();
      output_substring stdout s (offset + cut) (length - cut)
    end
    else output_substring stdout s offset length
  in
  let ppf = Format.make_formatter out_string (fun () -> flush stdout) in
  Fmt.set_utf_8 ppf true;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~ppf ~progress:`Tty
    (fun root ->
      let task = Console.Task.start root "pushing" in
      Console_eio.with_output ignore;
      if closing then begin
        Console.Task.succeed task;
        armed := true
      end
      else begin
        armed := true;
        Console.Task.succeed task
      end)

(* Lines printed outside a task and never flushed, then a mark written directly
   to the terminal. The output before the mark is what the display did not hold
   back. *)
let unflushed _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun _ ->
      for i = 1 to 100_000 do
        Fmt.pr "line %06d\n" i
      done;
      ignore (Unix.write_substring Unix.stdout "MARK\n" 0 5 : int))

let elsewhere _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Console.Task.start root "pushing" in
      Eio_unix.run_in_systhread (fun () -> Fmt.pr "from a thread@.");
      Console.Task.succeed task)

(* A whole line outside a task, long enough that the formatter writes it and
   then waits for a flush. The next line, in a task, must not take it along. *)
let held _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Fmt.pr "a held line %s\n" (String.make 100 '.');
      Console.Task.run root "work" (fun task ->
          Fmt.pr "a task line@.";
          Console.Task.fail task))

(* The program's first domain starts during the display. Format then gives each
   standard formatter one buffer per domain. Lines follow, outside a task and in
   a task that fails. After the display, the program prints whether a write on
   standard error waits for a flush, as Format's own writes do once a domain has
   started, instead of going directly to the channel. *)
let spawn _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Domain.join (Domain.spawn ignore);
      for i = 1 to 3 do
        Fmt.epr "spawned line %d@." i
      done;
      Console.Task.run root "work" (fun task ->
          Fmt.epr "a task line@.";
          Console.Task.fail task));
  let out = Format.pp_get_formatter_out_functions Fmt.stderr () in
  let before = pos_out stderr in
  out.out_string "after the run\n" 0 14;
  let held = pos_out stderr = before in
  out.out_flush ();
  Fmt.epr "held until a flush: %b@." held

(* Quiet mode keeps terminal output only for a failed or cancelled task. *)
let quiet ending _env =
  Logs.set_level (Some Logs.Info);
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ())
    ~progress:`Quiet ~label:"quiet header" (fun root ->
      Console.Task.run root "quiet work" (fun task ->
          Console.Task.log task "display line";
          Fmt.pr "stdout line@.";
          Fmt.epr "stderr line@.";
          Logs.info (fun m -> m "logs line");
          match ending with
          | "fail" -> Console.Task.fail task
          | "cancel" -> Console.Task.cancel task
          | _ -> ());
      Console.Task.within root (fun () -> Fmt.pr "root line@."))

(* SIGINT arrives while another domain computes and the display's domain sleeps,
   so the other domain can handle it. With [masked], the display's thread blocks
   SIGINT, so the other domain always handles it. *)
let sigint_beside ~masked env =
  let clock = Eio.Stdenv.mono_clock env in
  let stop = Atomic.make false in
  let worker =
    Domain.spawn (fun () ->
        while not (Atomic.get stop) do
          ignore (Sys.opaque_identity (ref 0))
        done)
  in
  if masked then
    ignore (Thread.sigmask Unix.SIG_BLOCK [ Sys.sigint ] : int list);
  at_exit (fun () -> Atomic.set stop true);
  Console_eio.run ~clock (fun root ->
      let task = Console.Task.start root "spinning" in
      for i = 1 to 100 do
        Console.Task.set_items task ~count:i ~total:100;
        if i = 10 then Unix.kill (Unix.getpid ()) Sys.sigint;
        Eio.Time.Mono.sleep clock 0.005
      done;
      Console.Task.succeed task);
  Atomic.set stop true;
  Domain.join worker

(* [exit] from another domain or from a thread while the display redraws. *)
let exit_drawing ~from env =
  let clock = Eio.Stdenv.mono_clock env in
  Console_eio.run ~clock (fun root ->
      let task = Console.Task.start root "drawing" in
      (match from with
      | `Domain ->
          ignore
            (Domain.spawn (fun () ->
                 Unix.sleepf 0.05;
                 exit 3)
              : unit Domain.t)
      | `Thread ->
          ignore
            (Thread.create
               (fun () ->
                 Thread.delay 0.05;
                 exit 4)
               ()
              : Thread.t));
      for i = 1 to 1000 do
        Console.Task.set_items task ~count:i ~total:1000;
        Eio.Time.Mono.sleep clock 0.001
      done;
      Console.Task.succeed task)

let display _env =
  let clock = Eio_mock.Clock.Mono.make () in
  let run () =
    Console_eio.run ~clock (fun root ->
        let task = Console.Task.start root "pushing" in
        Console.Task.log task "pushed 3 objects";
        Fmt.pr "a line of output@.";
        Console_eio.with_output ignore;
        match Sys.argv with
        | [| _; "sigint" |] -> Unix.kill (Unix.getpid ()) Sys.sigint
        | [| _; "exit" |] -> exit 3
        | [| _; "raise" |] -> failwith "push failed"
        | [| _; "cancel" |] -> Eio.Fiber.await_cancel ()
        | _ -> Console.Task.succeed task)
  in
  Eio.Fiber.first run Eio.Fiber.yield

let () =
  Eio_main.run @@ fun env ->
  match Sys.argv with
  | [| _; "repeated-signal"; first; second; cut; closing |] ->
      let signal = function "int" -> Sys.sigint | _ -> Sys.sigterm in
      repeated_signal ~first:(signal first) ~second:(signal second)
        ~cut:(int_of_string cut) ~closing:(bool_of_string closing) env
  | [| _; "quiet-ok" |] -> quiet "ok" env
  | [| _; "quiet-fail" |] -> quiet "fail" env
  | [| _; "quiet-cancel" |] -> quiet "cancel" env
  | [| _; "secret" |] -> secret env
  | [| _; "colourless" |] -> colourless env
  | [| _; "widgets" |] -> widgets env
  | [| _; "themed" |] -> themed env
  | [| _; "logs" |] -> logs ~fails:false env
  | [| _; "logs-failed" |] -> logs ~fails:true env
  | [| _; "suspend" |] -> suspend env
  | [| _; "orphaned-suspend" |] ->
      ignore (Unix.setsid () : int);
      suspend env
  | [| _; "unflushed" |] -> unflushed env
  | [| _; "held" |] -> held env
  | [| _; "elsewhere" |] -> elsewhere env
  | [| _; "spawn" |] -> spawn env
  | [| _; "sigint-masked" |] -> sigint_beside ~masked:true env
  | [| _; "sigint-beside" |] -> sigint_beside ~masked:false env
  | [| _; "exit-domain-drawing" |] -> exit_drawing ~from:`Domain env
  | [| _; "exit-thread-drawing" |] -> exit_drawing ~from:`Thread env
  | [| _; "secret-lost" |] -> secret_lost env
  | [| _; "cut-link" |] -> blocked_at ~cut:in_link interrupt env
  | [| _; "cut-sequence" |] -> blocked_at ~cut:in_sequence interrupt env
  | [| _; "stop-blocked" |] ->
      blocked_at
        ~block:(fun () -> Unix.sleepf 2.5)
        ~cut:in_sequence
        (fun () -> Unix.kill (Unix.getpid ()) Sys.sigtstp)
        env
  | [| _; "exit-thread-blocked" |] ->
      blocked_at ~cut:in_sequence (exit_from `Thread) env
  | [| _; "exit-domain-blocked" |] ->
      blocked_at ~cut:in_sequence (exit_from `Domain) env
  | [| _; "sigint-mid-write" |] -> mid_write ~blocked:false env
  | [| _; "sigint-blocked" |] -> mid_write ~blocked:true env
  | _ -> display env
