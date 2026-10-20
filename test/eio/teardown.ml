(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command that ends inside a live display the way its argument says: it
   returns, calls [exit] (a CLI's error path), raises to the toplevel, or has
   its fiber cancelled; or, with [secret], it is interrupted while it reads
   a password; or, with [colourless], it chose no colour before the display;
   or, with [widgets], it prints every widget before it; or, with [themed],
   it gave standard error a theme and chose nothing else; or, with [logs], it
   logs from a fiber its task forked; or, with [suspend], it is stopped by
   SIGTSTP, as Ctrl-Z does, and goes on once continued. It also prints a line on standard
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
       [ Table.column "a"; Table.column ~overflow:`Truncate "b" ]
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

let logs env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      Console.Display.with_task display "work" (fun task ->
          Console.Display.log task "an output line";
          Eio.Fiber.both
            (fun () -> Logs.warn (fun m -> m "a forked note"))
            Eio.Fiber.yield))

let suspend env =
  Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) (fun display ->
      let task = Console.Display.task display "pausing" in
      Console.Display.redraw display;
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      Console.Display.set_count task ~cur:1 ~total:2;
      Console.Display.redraw display;
      Console.Display.succeed task)

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
  | [| _; "logs" |] -> logs env
  | [| _; "suspend" |] -> suspend env
  | _ -> display env
