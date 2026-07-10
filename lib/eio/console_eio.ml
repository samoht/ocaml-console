module Anim = Anim
module Display = Display
module Spinner = Spinner
module Terminal = Terminal

(* Build a display ctx from the eio environment (formatter, the terminal
   geometry from {!Terminal}, and an explicit clock for timers and animation).
   On a terminal, restore the cursor when [f] returns, raises, or the process is
   interrupted with CTRL-C, so a display torn down by SIGINT never leaves the
   cursor hidden. *)
let run ~clock ?(ppf = Fmt.stdout) f =
  let now () = Eio.Time.now clock in
  let is_tty = Terminal.is_tty () in
  let ctx =
    Console.Display.ctx ~ppf ~now ~dimensions:Terminal.dimensions ~is_tty ()
  in
  if not is_tty then f ctx
  else
    let restore () = Terminal.show_cursor ppf in
    Terminal.on_interrupt restore (fun () ->
        Fun.protect ~finally:restore (fun () -> f ctx))
