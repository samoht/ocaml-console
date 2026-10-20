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
  | Some (fd, unix) when Console_tty.is_terminal unix -> Some fd
  | _ -> None

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

let run ~clock ?(ppf = Fmt.stderr) ?layout ?header f =
  match Session.find () with
  | Some display -> f display
  | None -> (
      Console_tty.ensure ();
      Session.claim ();
      let now () = Eio.Time.now clock in
      match
        match terminal ppf with
        | None ->
            (Console.Display.v ?layout ?header ~scope:Session.scope ~now ppf, [])
        | Some fd ->
            let size () = Terminal.dimensions (Some fd) in
            let display =
              Console.Display.v ?layout ?header ~size ~scope:Session.scope ~now
                (own ppf)
            in
            let routed =
              List.filter
                (fun std -> Option.is_some (terminal std))
                [ Fmt.stdout; Fmt.stderr ]
            in
            (display, routed)
      with
      | display, intercept -> Session.run ~clock ~intercept display f
      | exception e ->
          Session.release ();
          raise e)
