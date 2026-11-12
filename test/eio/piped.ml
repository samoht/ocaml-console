(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A program run by [Console_eio.run] on a pipe. It writes its results on
    standard output, and its display and Logs messages on standard error. The
    arguments select the case:
    - [n]: it does [n] steps. A large [n] writes far more than a pipe holds, so
      a reader that stops early closes the pipe while the program writes.
    - [bare n]: it prints the [n] results alone, without console.
    - [notes n]: as [n], with a Logs warning and a line on standard error after
      each step.
    - [exit]: it calls [exit] inside a task. An [at_exit] function, registered
      before, logs and prints.
    - [quiet-long] or [quiet-prompt]: in quiet mode, it leaves an unended line
      on standard error, then a task fails. The line is too long to hold, or is
      a question.
    - [json-stdout]: with JSON events, a task prints one line on standard output
      and one on standard error.
    - [sleeps n]: a task's two fibers sleep on {!Console_eio.clock}. One of them
      works [n] steps before each sleep.
    - [signal] or [sigterm]: SIGINT or SIGTERM arrives after a view raised.
    - [idle-signal file] or [idle-sigterm file]: SIGINT or SIGTERM arrives while
      the scheduler waits. The display writes to [file] through an Eio flow.
    - [exit-cancelled file]: a cancelled fiber calls [exit]. The display writes
      to [file] through an Eio flow.
    - [exit-view file]: a view calls [exit] each time it draws. The display
      writes to [file] through an Eio flow.
    - [size]: it prints the size of the terminal of standard error.
    - [width]: it prints the width of standard output, or [whole].
    - [print]: it prints a rule and a table above the rows, at the width the
      display uses.
    - [domain]: a task starts the program's first domain, then the program
      prints. *)

let step display i =
  Fmt.pr "result %d\n" i;
  Console.Task.run display (Fmt.str "step %d" i) (fun _ ->
      Logs.info (fun m -> m "working on %d" i))

let () =
  Logs.set_level (Some Logs.Info);
  Eio_main.run @@ fun env ->
  (* A clock that never moves, so every printed time is 0.0s. *)
  let clock =
    (Eio_mock.Clock.Mono.make () :> Eio.Time.Mono.ty Eio.Resource.t)
  in
  match Sys.argv with
  | [| _; "bare"; n |] ->
      for i = 1 to int_of_string n do
        Fmt.pr "result %d\n" i
      done
  | [| _; "notes"; n |] ->
      Console_eio.run ~clock (fun display ->
          for i = 1 to int_of_string n do
            step display i;
            Logs.warn (fun m -> m "after %d" i);
            Fmt.epr "note %d@." i
          done)
  | [| _; "exit" |] ->
      Logs.set_reporter (Logs_fmt.reporter ());
      at_exit (fun () ->
          Logs.warn (fun m -> m "logged at exit");
          Fmt.epr "printed at exit@.";
          Fmt.pr "result at exit@.");
      Console_eio.run ~clock (fun display ->
          Console.Task.run display "work" (fun _ ->
              Fmt.pr "result 1@.";
              exit 3))
  | [| _; "sleeps"; work |] ->
      let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
      let work () =
        for i = 1 to int_of_string work do
          ignore (Sys.opaque_identity (i * i))
        done
      in
      Console_eio.run ~clock ~progress:`Json (fun display ->
          Console.Task.run display "build" (fun _ ->
              Eio.Fiber.both
                (fun () ->
                  for _ = 1 to 4 do
                    work ();
                    Eio.Time.Mono.sleep clock 0.1
                  done)
                (fun () ->
                  for _ = 1 to 3 do
                    Eio.Time.Mono.sleep clock 0.13
                  done)))
  | [| _; "quiet-long" |] ->
      Console_eio.run ~clock ~progress:`Quiet (fun display ->
          Fmt.epr "%s%!" (String.make 70_000 'x');
          Console.Task.run display "work" Console.Task.fail)
  | [| _; "quiet-prompt" |] ->
      Console_eio.run ~clock ~progress:`Quiet (fun display ->
          let prompt =
            Console_eio.Prompt.v ~interactive:true (Eio.Stdenv.stdin env)
          in
          (match Console_eio.Prompt.line prompt "name? " with
          | Ok _ -> ()
          | Error _ -> failwith "quiet-prompt: no answer");
          Console.Task.run display "work" Console.Task.fail)
  | [| _; "json-stdout" |] ->
      Console_eio.run ~clock ~progress:`Json (fun display ->
          Console.Task.run display "build" (fun _ ->
              Fmt.pr "from stdout@.";
              Fmt.epr "from stderr@."))
  | [| _; ("signal" | "sigterm") as name |] ->
      let armed = ref false in
      let view _ =
        Console.View.fit (fun _ ->
            if !armed then failwith "view" else Console.View.text "row")
      in
      Console_eio.run ~clock ~progress:`Tty ~view (fun _ ->
          armed := true;
          Console_eio.print (Console.View.text "line");
          Unix.kill (Unix.getpid ())
            (if name = "sigterm" then Sys.sigterm else Sys.sigint);
          Unix.sleepf 2.;
          Fmt.pr "still running@.")
  | [| _; ("idle-signal" | "idle-sigterm") as name; file |] ->
      let path = Eio.Path.(Eio.Stdenv.cwd env / file) in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path @@ fun out ->
      let write s o l = Eio.Flow.copy_string (String.sub s o l) out in
      let ppf = Format.make_formatter write ignore in
      let clock = Eio.Stdenv.mono_clock env in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          ignore (Console.Task.start root "t");
          let pid = Unix.getpid () in
          ignore
            (Thread.create
               (fun () ->
                 Thread.delay 0.2;
                 Unix.kill pid
                   (if name = "idle-sigterm" then Sys.sigterm else Sys.sigint))
               ());
          Eio.Time.Mono.sleep clock 3.;
          Fmt.pr "still running@.")
  | [| _; "exit-cancelled"; file |] ->
      let path = Eio.Path.(Eio.Stdenv.cwd env / file) in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path @@ fun out ->
      let write s o l = Eio.Flow.copy_string (String.sub s o l) out in
      let ppf = Format.make_formatter write ignore in
      Console_eio.run ~clock ~ppf ~progress:`Tty (fun root ->
          ignore (Console.Task.start root "t");
          Eio.Fiber.first
            (fun () ->
              Fun.protect ~finally:(fun () -> exit 3) Eio.Fiber.await_cancel)
            ignore)
  | [| _; "exit-view"; file |] ->
      let path = Eio.Path.(Eio.Stdenv.cwd env / file) in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path @@ fun out ->
      let write s o l = Eio.Flow.copy_string (String.sub s o l) out in
      let ppf = Format.make_formatter write ignore in
      let armed = ref false in
      let view _ =
        Console.View.fit (fun _ ->
            if !armed then exit 3 else Console.View.text "row")
      in
      Console_eio.run ~clock ~ppf ~progress:`Tty ~view (fun _ ->
          armed := true;
          Console_eio.print (Console.View.text "line"))
  | [| _; "size" |] -> (
      match Console_tty.size Fmt.stderr with
      | Some size ->
          let w, h = size () in
          Fmt.pr "%dx%d@." w h
      | None -> Fmt.pr "no terminal@.")
  | [| _; "width" |] -> (
      match Console_tty.width Fmt.stdout with
      | Some w -> Fmt.pr "%d@." w
      | None -> Fmt.pr "whole@.")
  | [| _; "print" |] ->
      Console_eio.run ~clock (fun _ ->
          Console_eio.print (Console.Rule.view ~glyph:"-" ());
          Console_eio.print
            Console.(
              Table.view ~border:Border.ascii
                [ Table.column (View.text "package") ]
                [
                  [
                    View.text
                      "a description long enough to wrap in a narrow table";
                  ];
                ]))
  | [| _; "domain" |] ->
      Console_tty.setup ();
      Console_eio.run ~clock (fun display ->
          Console.Task.run display "spawn" (fun _ ->
              Domain.join (Domain.spawn ignore));
          Fmt.epr "after the first domain@.")
  | [| _; n |] ->
      Console_eio.run ~clock (fun display ->
          for i = 1 to int_of_string n do
            step display i
          done)
  | _ -> invalid_arg "piped [bare] count"
