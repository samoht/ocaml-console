(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

let getenv = Sys.getenv_opt
let decided = Atomic.make false

(* Fmt keeps what a program chose for a formatter in tag functions it
   installs on the first choice, so a formatter whose tag functions are still
   Format's own, but for its theme, has had nothing chosen for it. *)
let chosen ppf = not (Console.Theme.unset ppf)

let decide ?style_renderer ~only () =
  List.iter
    (fun (ppf, fd) ->
      if only ppf then begin
        let renderer =
          match style_renderer with
          | Some r -> r
          | None -> Colour.renderer ~getenv ~is_tty:(Terminal.is_tty fd)
        in
        Fmt.set_style_renderer ppf renderer;
        Fmt.set_utf_8 ppf (Colour.utf_8 ~getenv);
        Console.Color.set_depth ppf (Colour.depth ~getenv)
      end)
    [ (Fmt.stdout, Eio_unix.Fd.stdout); (Fmt.stderr, Eio_unix.Fd.stderr) ];
  Atomic.set decided true

let setup ?style_renderer () = decide ?style_renderer ~only:(Fun.const true) ()

(* The descriptor a formatter writes to, when it is one of the two the
   standard library opens on them. *)
let descriptor ppf =
  if ppf == Fmt.stdout then Some Eio_unix.Fd.stdout
  else if ppf == Fmt.stderr then Some Eio_unix.Fd.stderr
  else None

let terminal ppf =
  match descriptor ppf with
  | Some fd when Terminal.is_tty fd && not (Colour.dumb ~getenv) -> Some fd
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
      if not (Atomic.get decided) then
        decide ~only:(fun ppf -> not (chosen ppf)) ();
      Session.claim ();
      let now () = Eio.Time.now clock in
      match
        match terminal ppf with
        | None -> (Console.Display.v ?layout ?header ~now ppf, [])
        | Some fd ->
            let size () = Terminal.dimensions (Some fd) in
            let display =
              Console.Display.v ?layout ?header ~size ~now (own ppf)
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
