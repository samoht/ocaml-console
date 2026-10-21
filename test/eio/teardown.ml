(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command that ends inside a live display the way its argument says: it
   returns, calls [exit] (a CLI's error path), raises to the toplevel, or has
   its fiber cancelled; or, with [secret], it is interrupted while it reads
   a password; or, with [colourless], it chose no colour before the display;
   or, with [widgets], it prints every widget before it; or, with [themed],
   it gave standard error a theme and chose nothing else; or, with [logs] or [logs-failed], its
   task prints and logs, then succeeds or fails; or, with [suspend], it is stopped by
   SIGTSTP, as Ctrl-Z does, and goes on once continued; or, with [unflushed], it
   prints lines it never flushes; or, with [held], it prints a line outside a
   task that it does not flush, then one in a task that fails. It also prints a line on standard
   output, which goes above the region, and its task a line of output. *)

let secret env =
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  Eio.Fiber.both
    (fun () -> ignore (Console_eio.Prompt.secret prompt "password: "))
    (fun () ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.2;
      Unix.kill (Unix.getpid ()) Sys.sigint)

(* The program chose no colour itself, as a [--color=never] does, before
   it runs a display. *)
let colourless env =
  Fmt.set_style_renderer Fmt.stdout `None;
  Fmt.set_style_renderer Fmt.stderr `None;
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      Console.Display.succeed (Console.Display.task display "plain"))

(* Every widget, on standard output once [setup] has looked at the locale,
   then a display with a bar. *)
let widgets env =
  let open Console in
  Console_tty.setup ();
  let long = Span.text (String.concat " " (List.init 20 (Fun.const "word"))) in
  let leaf = Tree.v long [] in
  Fmt.pr "%a@.%a@.%a@.%a@.%a@." Table.pp
    (Table.v
       [
         Table.column (Span.text "a");
         Table.column ~overflow:`Truncate (Span.text "b");
       ]
       [ [ Span.text "1"; long ] ])
    Panel.pp (Panel.v ~title:long long) Tree.pp
    (Tree.v long [ Tree.v long [ leaf ]; leaf ])
    Rule.pp (Rule.v ~label:long ()) Bar.pp
    (Bar.v ~brackets:true ~width:10 0.55);
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      let task = Display.task display "counting" in
      Display.set_count task ~cur:1 ~total:3;
      Display.redraw display;
      Display.succeed task)

(* The program chose a theme, and no colour, before the display. *)
let themed env =
  Console.Theme.set Fmt.stderr Console.Theme.nord;
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      Console.Display.succeed (Console.Display.task display "themed"))

(* A task's output, a line on each standard output, one within the root task
   and a Logs message from a fiber the task forked; with [fails], the task
   fails. *)
let logs ~fails env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      Console.Display.with_task display "work" (fun task ->
          Console.Display.log task "an output line";
          Fmt.pr "a result line@.";
          Fmt.epr "a note line@.";
          Console.Display.within (Console.Display.root display) (fun () ->
              Fmt.pr "a root line@.");
          Eio.Fiber.both
            (fun () -> Logs.warn (fun m -> m "a forked note"))
            Eio.Fiber.yield;
          if fails then Console.Display.fail task))

let suspend env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      let task = Console.Display.task display "pausing" in
      Console.Display.redraw display;
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      Console.Display.set_count task ~cur:1 ~total:2;
      Console.Display.redraw display;
      Console.Display.succeed task)

(* Lines printed outside a task and never flushed, then a mark written
   straight to the terminal: what came out before the mark is what the
   display did not hold back. *)
let unflushed env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun _ ->
      for i = 1 to 100_000 do
        Fmt.pr "line %06d\n" i
      done;
      ignore (Unix.write_substring Unix.stdout "MARK\n" 0 5 : int))

(* A whole line outside a task, long enough that the formatter writes it,
   waits for a flush; the task's line that comes next must not take it
   along. *)
let held env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      Fmt.pr "a held line %s\n" (String.make 100 '.');
      Console.Display.with_task display "work" (fun task ->
          Fmt.pr "a task line@.";
          Console.Display.fail task))

let display env =
  let clock = Eio.Stdenv.mono_clock env in
  let run () =
    Console_eio.run ~clock (fun display ->
        let task = Console.Display.task display "pushing" in
        Console.Display.log task "pushed 3 objects";
        Fmt.pr "a line of output@.";
        Console.Display.redraw display;
        match Sys.argv with
        | [| _; "sigint" |] -> Unix.kill (Unix.getpid ()) Sys.sigint
        | [| _; "exit" |] -> exit 3
        | [| _; "raise" |] -> failwith "push failed"
        | [| _; "cancel" |] -> Eio.Fiber.await_cancel ()
        | _ -> Console.Display.succeed task)
  in
  Eio.Fiber.first run Eio.Fiber.yield

let () =
  Eio_main.run @@ fun env ->
  match Sys.argv with
  | [| _; "secret" |] -> secret env
  | [| _; "colourless" |] -> colourless env
  | [| _; "widgets" |] -> widgets env
  | [| _; "themed" |] -> themed env
  | [| _; "logs" |] -> logs ~fails:false env
  | [| _; "logs-failed" |] -> logs ~fails:true env
  | [| _; "suspend" |] -> suspend env
  | [| _; "unflushed" |] -> unflushed env
  | [| _; "held" |] -> held env
  | _ -> display env
