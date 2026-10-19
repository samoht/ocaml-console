(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

let getenv = Sys.getenv_opt
let decided = Atomic.make false

let setup ?style_renderer () =
  let set ppf fd =
    let renderer =
      match style_renderer with
      | Some r -> r
      | None -> Colour.renderer ~getenv ~is_tty:(Terminal.is_tty fd)
    in
    Fmt.set_style_renderer ppf renderer;
    Console.Color.set_depth ppf (Colour.depth ~getenv)
  in
  set Fmt.stdout Eio_unix.Fd.stdout;
  set Fmt.stderr Eio_unix.Fd.stderr;
  Atomic.set decided true

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
   [ppf] and in its colours, so that [ppf] itself can be routed above the
   region. *)
let own ppf =
  let own =
    Format.formatter_of_out_functions
      (Format.pp_get_formatter_out_functions ppf ())
  in
  Fmt.set_style_renderer own (Fmt.style_renderer ppf);
  Console.Color.set_depth own (Console.Color.depth ppf);
  own

let run ~clock ?(ppf = Fmt.stderr) ?theme ?layout ?header f =
  match Session.find () with
  | Some display -> f display
  | None -> (
      if not (Atomic.get decided) then setup ();
      Session.claim ();
      let now () = Eio.Time.now clock in
      match
        match terminal ppf with
        | None -> (Console.Display.v ?theme ?layout ?header ~now ppf, [])
        | Some fd ->
            let size () = Terminal.dimensions (Some fd) in
            let display =
              Console.Display.v ?theme ?layout ?header ~size ~now (own ppf)
            in
            let routed =
              List.filter_map
                (fun std -> Option.map (fun _ -> std) (terminal std))
                [ Fmt.stdout; Fmt.stderr ]
            in
            (display, routed)
      with
      | display, intercept -> Session.run ~clock ~intercept display f
      | exception e ->
          Session.release ();
          raise e)
