(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A program under [Console_eio.run] on a pipe: its results on standard output,
    its display and Logs messages on standard error. Given a count, it does that
    many steps, enough to write far more than a pipe holds, so that a reader
    that stops early closes the pipe under it; with [bare], it prints the
    results alone, without console; with [exit], it calls [exit] inside a task,
    and an [at_exit] function it registered first logs and prints. *)

let step display i =
  Fmt.pr "result %d\n" i;
  Console.Display.with_task display (Fmt.str "step %d" i) (fun _ ->
      Logs.info (fun m -> m "working on %d" i))

let () =
  Logs.set_level (Some Logs.Info);
  Eio_main.run @@ fun _ ->
  (* A clock that does not move, so that every time printed is 0.0s. *)
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
          Console.Display.with_task display "work" (fun _ ->
              Fmt.pr "result 1@.";
              exit 3))
  | [| _; n |] ->
      Console_eio.run ~clock (fun display ->
          for i = 1 to int_of_string n do
            step display i
          done)
  | _ -> invalid_arg "piped [bare] count"
