(* A library may itself be written as a complete Console application and then
   be called by another Console application.  Re-entering [run] in that case
   must not try to claim the terminal a second time: the current display is a
   fiber-local capability, inherited by child fibers, so nested callers simply
   contribute tasks and events to the outer session. *)
let current : Console.Display.t Eio.Fiber.key = Eio.Fiber.create_key ()

let run ~clock ?ppf ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Eio.Fiber.get current with
  | Some display -> f display
  | None -> (
      let ppf = Option.value ppf ~default:Fmt.stdout in
      let is_tty = Terminal.is_tty () in
      let ctx =
        Console.Display.ctx ~ppf
          ~now:(fun () -> Eio.Time.now clock)
          ~dimensions:Terminal.dimensions ~is_tty ()
      in
      let restore =
        if is_tty then Some (fun () -> Terminal.show_cursor ppf) else None
      in
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
