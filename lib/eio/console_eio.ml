module Prompt = Prompt

(* What a terminal is and how big it is are the same questions [run] asks to
   build its own context, so a caller choosing a layout for itself asks them
   here rather than reaching for Unix. Terminal stays private: the rest of it
   -- the cursor escapes, the interrupt handler -- is the driver's business. *)
let is_tty = Terminal.is_tty
let dimensions () = Terminal.dimensions (Some Eio_unix.Fd.stdout)

(* The descriptor a formatter writes to, where the formatter says so by being
   one of the two the standard library opens on them. *)
let descriptor ?fd ppf =
  match fd with
  | Some _ -> fd
  | None ->
      if ppf == Fmt.stdout then Some Unix.stdout
      else if ppf == Fmt.stderr then Some Unix.stderr
      else None

(* A region moves the cursor, which a terminal whose TERM is dumb has none of
   to move. *)
let region_on fd =
  is_tty fd
  && not (Option.equal String.equal (Sys.getenv_opt "TERM") (Some "dumb"))

let auto ?fd ppf =
  match descriptor ?fd ppf with
  | Some fd when region_on fd -> `Activity
  | Some _ | None -> `History

let frame_drawn display n =
  let promise, resolver = Eio.Promise.create () in
  Console.Display.on_frame display n (fun () ->
      ignore (Eio.Promise.try_resolve resolver () : bool));
  promise

let setup ?style_renderer () =
  let set ppf fd =
    Fmt.set_style_renderer ppf
      (Console.style_renderer ?renderer:style_renderer ~getenv:Sys.getenv_opt
         ~is_tty:(Unix.isatty fd) ())
  in
  set Fmt.stdout Unix.stdout;
  set Fmt.stderr Unix.stderr;
  Console.Color.set_depth (Console.Color.depth_of_env Sys.getenv_opt)

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

(* The context [run] builds for the descriptor [ppf] writes to, and the cursor
   it then owes a terminal there. The geometry is that terminal's, read for
   the span of [sw]. The wait is the clock's own sleep: under
   CONSOLE_FROZEN_CLOCK the reading stops and the pace does not, so a refresh
   still turns. *)
let context ~sw ~clock ~ppf ~fd =
  let ppf = Option.value ppf ~default:Fmt.stdout in
  let tty =
    match descriptor ?fd ppf with
    | Some fd when is_tty fd -> Some fd
    | Some _ | None -> None
  in
  let window = Option.map (Eio_unix.Fd.of_unix ~sw ~close_unix:false) tty in
  let ctx =
    Console.Display.ctx ~ppf ~now:(display_clock clock)
      ~wait:(Eio.Time.sleep clock)
      ~dimensions:(fun () -> Terminal.dimensions window)
      ~is_tty:(auto ?fd ppf = `Activity)
      ()
  in
  let restore =
    if Option.is_some tty then Some Console.Display.restore_terminal else None
  in
  (ctx, restore)

(* Only the outermost call opens a display and asks for its context; a call
   under it contributes to the display that stands. *)
let session ~context ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Eio.Fiber.get current with
  | Some display -> f display
  | None -> (
      Eio.Switch.run @@ fun sw ->
      let ctx, restore = context ~sw in
      let run () =
        Display.run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header
          (fun display ->
            Eio.Fiber.with_binding current display (fun () -> f display))
      in
      match restore with
      | None -> run ()
      | Some restore ->
          Terminal.on_interrupt restore (fun () ->
              Fun.protect ~finally:restore run))

let run ~clock ?ppf ?fd ?mode ?theme ?palette ?bar ?width ?height ?header f =
  session
    ~context:(fun ~sw -> context ~sw ~clock ~ppf ~fd)
    ?mode ?theme ?palette ?bar ?width ?height ?header f

(* A caller that supplies its own context has answered the output, clock,
   geometry and terminal questions already, and with them the question of who
   owns the terminal: [run_on] configures none and restores none, because the
   surface it was handed may not be one. *)
let run_on ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  session
    ~context:(fun ~sw:_ -> (ctx, None))
    ?mode ?theme ?palette ?bar ?width ?height ?header f
