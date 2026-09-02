module Prompt = Prompt

(* What a terminal is and how big it is are the same questions [run] asks to
   build its own context, so a caller choosing a layout for itself asks them
   here rather than reaching for Unix. Terminal stays private: the rest of it
   -- the cursor escapes, the interrupt handler -- is the driver's business. *)
let is_tty = Terminal.is_tty
let dimensions = Terminal.dimensions

(* A library may itself be written as a complete Console application and then
   be called by another Console application.  Re-entering [run] in that case
   must not try to claim the terminal a second time: the current display is a
   fiber-local capability, inherited by child fibers, so nested callers simply
   contribute tasks and events to the outer session. *)
let current : Console.Display.t Eio.Fiber.key = Eio.Fiber.create_key ()

(* An elapsed time is the one part of a rendered display the machine writes
   rather than the program, so a captured transcript that shows one pins the
   speed of the host that recorded it and goes red on a slower host.
   CONSOLE_FROZEN_CLOCK, set to any non-empty value, stops the display clock at
   zero: every elapsed then renders as 0.0s and the transcript asserts the
   program's own output and nothing about the machine. It replaces the real
   clock read here and only that one, since a caller that builds a context with
   a clock of its own has already said what time the display reads. *)
let display_clock clock =
  match Sys.getenv_opt "CONSOLE_FROZEN_CLOCK" with
  | None | Some "" -> fun () -> Eio.Time.now clock
  | Some _ -> fun () -> 0.

(* The context [run] would build for itself, and the terminal it then owes a
   restored cursor. A caller that supplies its own context has answered the
   output, clock, geometry and terminal questions already, and with them the
   question of who owns the terminal: [run] configures none and restores none,
   because the surface it was handed may not be one. *)
let context ~clock ~ppf ~ctx =
  match ctx with
  | Some ctx ->
      if Option.is_some ppf then
        invalid_arg
          "Console_eio.run: ppf and ctx are both given; a context already \
           carries its formatter";
      (ctx, None)
  | None ->
      let ppf = Option.value ppf ~default:Fmt.stdout in
      let is_tty = is_tty () in
      let ctx =
        Console.Display.ctx ~ppf ~now:(display_clock clock) ~dimensions ~is_tty
          ()
      in
      let restore =
        if is_tty then Some (fun () -> Terminal.show_cursor ppf) else None
      in
      (ctx, restore)

let run ~clock ?ppf ?ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Eio.Fiber.get current with
  | Some display -> f display
  | None -> (
      let ctx, restore = context ~clock ~ppf ~ctx in
      let run () =
        Eio.Switch.run (fun sw ->
            Display.run ~sw ~clock ~ctx ?mode ?theme ?palette ?bar ?width
              ?height ?header (fun display ->
                Eio.Fiber.with_binding current display (fun () -> f display)))
      in
      match restore with
      | None -> run ()
      | Some restore ->
          Terminal.on_interrupt restore (fun () ->
              Fun.protect ~finally:restore run))
