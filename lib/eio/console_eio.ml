(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

(* The descriptor a formatter writes to, when it is one of the two the
   standard library opens on them. *)
let descriptor ppf =
  if ppf == Fmt.stdout then Some (Eio_unix.Fd.stdout, Unix.stdout)
  else if ppf == Fmt.stderr then Some (Eio_unix.Fd.stderr, Unix.stderr)
  else None

let terminal ppf =
  match descriptor ppf with
  | Some (fd, unix) when Console_tty.is_terminal unix -> Some (fd, unix)
  | _ -> None

(* Two descriptors on one terminal are on one device. *)
let device unix =
  match Unix.fstat unix with
  | st -> Some st.Unix.st_rdev
  | exception Unix.Unix_error _ -> None

(* The standard formatters on the display's terminal, whose lines go above
   its region; one on another terminal goes out as it is written. *)
let routed unix =
  let on_display std =
    match terminal std with
    | Some (_, u) -> Option.equal Int.equal (device u) (device unix)
    | None -> false
  in
  List.filter on_display [ Fmt.stdout; Fmt.stderr ]

(* The file, pipe or device a descriptor writes to. *)
let file unix =
  match Unix.fstat unix with
  | st -> Some (st.Unix.st_dev, st.Unix.st_ino)
  | exception Unix.Unix_error _ -> None

(* The standard formatters that write where [unix] does. *)
let sharing unix =
  let shares std =
    match (descriptor std, file unix) with
    | Some (_, u), Some f -> file u = Some f
    | _ -> false
  in
  List.filter shares [ Fmt.stdout; Fmt.stderr ]

(* The display writes on a formatter of its own, to the same output as
   [ppf] and with the same tag functions, which hold its colours, its theme
   and whether it is UTF-8, as [Fmt.with_buffer ~like] does, so that [ppf]
   itself can be routed above the region. *)
let own ppf =
  let own =
    Format.formatter_of_out_functions
      (Format.pp_get_formatter_out_functions ppf ())
  in
  Format.pp_set_formatter_stag_functions own
    (Format.pp_get_formatter_stag_functions ppf ());
  own

(* The cursor of the terminal on [unix], shown or hidden. *)
let cursor unix visible =
  let s = if visible then "\027[?25h" else "\027[?25l" in
  try ignore (Unix.write_substring unix s 0 (String.length s))
  with Unix.Unix_error _ -> ()

(* The seconds since the display started, on [clock]. *)
let seconds clock =
  let start = Eio.Time.Mono.now clock in
  fun () ->
    Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) *. 1e-9

let deterministic () =
  match Sys.getenv_opt "CONSOLE_DETERMINISTIC_CLOCK" with
  | None | Some "" -> false
  | Some _ -> true

(* A clock that reads the latest time one of its sleeps ended at, and
   whose sleeps wait on [real] until that time. Each sleep ends at the time
   the clock read when it started plus its duration, however late [real]
   wakes it. *)
module Sleeps = struct
  type t = { real : Eio.Time.Mono.ty Eio.Resource.t; mutable now : Mtime.t }
  type time = Mtime.t

  let now t = t.now

  let sleep_until t time =
    Eio.Time.Mono.sleep_until t.real time;
    if Mtime.is_later time ~than:t.now then t.now <- time
end

type (_, _, _) Eio.Resource.pi +=
  | Real :
      ( 't,
        't -> Eio.Time.Mono.ty Eio.Resource.t,
        [> `Console_sleeps ] )
      Eio.Resource.pi

let sleeps =
  Eio.Resource.handler
    (H (Real, fun t -> t.Sleeps.real)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Sleeps)))

let clock c =
  let real = (c :> Eio.Time.Mono.ty Eio.Resource.t) in
  if deterministic () then
    Eio.Resource.T ({ Sleeps.real; now = Eio.Time.Mono.now real }, sleeps)
  else real

(* The clock that [clock] waits on, if it is one of {!clock}'s. *)
let real (Eio.Resource.T (t, ops)) =
  Option.map (fun real -> real t) (Eio.Resource.get_opt ops Real)

(* What the display reads and what its redraws wait on. Under
   CONSOLE_DETERMINISTIC_CLOCK the display reads a clock of {!clock}, whose
   time its own redraws must not move, or a clock stopped at zero. *)
let reading clock =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  if not (deterministic ()) then (seconds clock, clock)
  else
    match real clock with
    | Some real -> (seconds clock, real)
    | None -> ((fun () -> 0.), clock)

(* The display, the formatters whose lines it takes and what shows or hides
   the cursor. On a terminal it takes the standard formatters on that
   terminal; quiet or as events, those that write where [ppf] does, so
   that a task's output is printed only if it fails, or is an event; drawn
   on another output, [ppf] alone. *)
let display ~progress ~now ?layout ?summary ?view ?keep ppf =
  let scope = Session.scope in
  let live ~size ~intercept ~cursor =
    ( Display_driver.v ?layout ?summary ?view ?keep ~size ~scope ~now (own ppf),
      intercept,
      cursor )
  in
  let on_terminal () =
    Option.map
      (fun (fd, unix) ->
        live
          ~size:(fun () -> Terminal.dimensions (Some fd))
          ~intercept:(routed unix) ~cursor:(cursor unix))
      (terminal ppf)
  in
  let lines form =
    match (form, descriptor ppf) with
    | (`Json | `Quiet), Some (_, unix) ->
        ( Display_driver.v ?layout ?summary ?view ?keep ~progress:form ~scope
            ~now (own ppf),
          sharing unix,
          ignore )
    | _ ->
        ( Display_driver.v ?layout ?summary ?view ?keep ~progress:form ~scope
            ~now ppf,
          [],
          ignore )
  in
  match progress with
  | `Auto -> ( match on_terminal () with Some d -> d | None -> lines `Plain)
  | `Tty -> (
      match on_terminal () with
      | Some d -> d
      | None ->
          let cursor =
            match descriptor ppf with
            | Some (_, unix) -> cursor unix
            | None -> ignore
          in
          let intercept =
            match descriptor ppf with Some _ -> [ ppf ] | None -> []
          in
          live ~size:(fun () -> Terminal.dimensions None) ~intercept ~cursor)
  | (`Plain | `Quiet | `Json) as form -> lines form

(* A display of its own, which [f] is given. *)
let open_display ~clock ~ppf ?layout ?summary ?view ?keep ~progress f =
  Console_tty.ensure ();
  Session.await_release ();
  Session.claim ();
  let now, redraws = reading clock in
  match display ~progress ~now ?layout ?summary ?view ?keep ppf with
  | display, intercept, cursor ->
      Session.run ~cursor
        ~sleep:(Eio.Time.Mono.sleep redraws)
        ~intercept display f
  | exception e ->
      Session.release ();
      raise e

let run ~clock ?(ppf = Fmt.stderr) ?layout ?summary ?view ?keep
    ?(progress = `Auto) f =
  match Session.find () with
  | Some display -> f display
  | None -> (
      match Session.join f with
      | Some v -> v
      | None ->
          open_display ~clock ~ppf ?layout ?summary ?view ?keep ~progress f)
