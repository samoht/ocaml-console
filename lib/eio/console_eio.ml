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

(* A clock stopped at zero, under CONSOLE_FROZEN_CLOCK; the redraws still
   wait on [clock]. *)
let reading clock =
  match Sys.getenv_opt "CONSOLE_FROZEN_CLOCK" with
  | None | Some "" -> seconds clock
  | Some _ -> fun () -> 0.

let run ~clock ?(ppf = Fmt.stderr) ?layout ?header ?(plain = false) f =
  match Session.find () with
  | Some display -> f display
  | None -> (
      Console_tty.ensure ();
      Session.claim ();
      let now = reading clock in
      match
        match if plain then None else terminal ppf with
        | None ->
            ( Display_driver.v ?layout ?header ~scope:Session.scope ~now ppf,
              [],
              ignore )
        | Some (fd, unix) ->
            let size () = Terminal.dimensions (Some fd) in
            let display =
              Display_driver.v ?layout ?header ~size ~scope:Session.scope ~now
                (own ppf)
            in
            (display, routed unix, cursor unix)
      with
      | display, intercept, cursor ->
          Session.run ~cursor
            ~sleep:(Eio.Time.Mono.sleep clock)
            ~intercept display f
      | exception e ->
          Session.release ();
          raise e)
