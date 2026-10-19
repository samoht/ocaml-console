module Prompt = Prompt

(* The descriptor a formatter writes to, where the formatter says so by being
   one of the two the standard library opens on them. *)
let descriptor ?fd ppf =
  match fd with
  | Some _ -> fd
  | None ->
      if ppf == Fmt.stdout then Some Eio_unix.Fd.stdout
      else if ppf == Fmt.stderr then Some Eio_unix.Fd.stderr
      else None

(* A region moves the cursor, which a terminal whose TERM is dumb has none of
   to move. *)
let region_on fd =
  Terminal.is_tty fd
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

(* The formatters {!setup} configured: their renderer is the caller's answer,
   even when it is [`None]. *)
let configured : Format.formatter list Atomic.t = Atomic.make []

let setup ?style_renderer () =
  let set ppf fd =
    Fmt.set_style_renderer ppf
      (Console.style_renderer ?renderer:style_renderer ~getenv:Sys.getenv_opt
         ~is_tty:(Terminal.is_tty fd) ())
  in
  set Fmt.stdout Eio_unix.Fd.stdout;
  set Fmt.stderr Eio_unix.Fd.stderr;
  Atomic.set configured [ Fmt.stdout; Fmt.stderr ]

(* The renderer [run] draws [ppf] with when nobody chose one: what {!setup}
   would choose for the terminal [tty]. [None] keeps the caller's. *)
let renderer ppf ~tty =
  if List.memq ppf (Atomic.get configured) || Fmt.style_renderer ppf = `Ansi_tty
  then None
  else
    Some
      (Console.style_renderer ~getenv:Sys.getenv_opt
         ~is_tty:(Option.is_some tty) ())

let with_renderer renderer f =
  match renderer with
  | None -> f ()
  | Some (ppf, renderer) ->
      let old = Fmt.style_renderer ppf in
      Fmt.set_style_renderer ppf renderer;
      Fun.protect ~finally:(fun () -> Fmt.set_style_renderer ppf old) f

(* A library may itself be written as a complete Console application and then
   be called by another Console application.  Re-entering [run] in that case
   must not try to claim the terminal a second time: the current display is a
   fiber-local capability, inherited by child fibers, so nested callers simply
   contribute tasks and events to the outer session. *)
let current : Console.Display.t Eio.Fiber.key = Eio.Fiber.create_key ()

(* The context [run] builds for the descriptor [ppf] writes to, what gives a
   terminal there its cursor back when a signal ends the process under the
   display, and the renderer it draws with. The geometry is that terminal's. *)
let context ~clock ~ppf ~fd =
  let ppf = Option.value ppf ~default:Fmt.stdout in
  let tty =
    match descriptor ?fd ppf with
    | Some fd when Terminal.is_tty fd -> Some fd
    | Some _ | None -> None
  in
  let ctx =
    Console.Display.ctx
      ~depth:(Console.Color.depth_of_env Sys.getenv_opt)
      ~ppf
      ~now:(fun () -> Eio.Time.now clock)
      ~wait:(Eio.Time.sleep clock)
      ~dimensions:(fun () -> Terminal.dimensions tty)
      ~is_tty:(auto ?fd ppf = `Activity)
      ()
  in
  let restore =
    if Option.is_some tty then Some Console.Display.restore else None
  in
  (ctx, restore, Option.map (fun r -> (ppf, r)) (renderer ppf ~tty))

(* Only the outermost call opens a display and asks for its context; a call
   under it contributes to the display that stands. *)
let session ~context ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Eio.Fiber.get current with
  | Some display -> f display
  | None -> (
      let ctx, restore, renderer = context () in
      let run () =
        with_renderer renderer @@ fun () ->
        Display.run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header
          (fun display ->
            Eio.Fiber.with_binding current display (fun () -> f display))
      in
      match restore with
      | None -> run ()
      | Some restore -> Terminal.on_interrupt restore run)

let run ~clock ?ppf ?fd ?mode ?theme ?palette ?bar ?width ?height ?header f =
  session
    ~context:(fun () -> context ~clock ~ppf ~fd)
    ?mode ?theme ?palette ?bar ?width ?height ?header f

(* A caller that supplies its own context has answered the output, clock,
   geometry and terminal questions already, and with them the question of who
   owns the terminal: [run_on] configures none and restores none, because the
   surface it was handed may not be one. *)
let run_on ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  session
    ~context:(fun () -> (ctx, None, None))
    ?mode ?theme ?palette ?bar ?width ?height ?header f
