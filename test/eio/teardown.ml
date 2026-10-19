(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command that ends inside a live display the way its argument says: it
   returns, calls [exit] (a CLI's error path), raises to the toplevel, or has
   its fiber cancelled; or, with [secret], it is interrupted while it reads
   a password; or, with [colourless], it chose no colour before the display. It also prints a line on standard output, which goes
   above the region. *)

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
  Console_eio.run ~clock:(Eio.Stdenv.clock env) (fun display ->
      Console.Display.succeed (Console.Display.task display "plain"))

let display env =
  let clock = Eio.Stdenv.clock env in
  let run () =
    Console_eio.run ~clock (fun display ->
        let task = Console.Display.task display "pushing" in
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
  | _ -> display env
