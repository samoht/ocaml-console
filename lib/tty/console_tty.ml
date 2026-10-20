(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let getenv = Sys.getenv_opt
let outputs = [ (Fmt.stdout, Unix.stdout); (Fmt.stderr, Unix.stderr) ]

let set ?style_renderer ?utf_8 (ppf, fd) =
  let renderer =
    match style_renderer with
    | Some r -> r
    | None -> Colour.renderer ~getenv ~is_tty:(Unix.isatty fd)
  in
  Fmt.set_style_renderer ppf renderer;
  Fmt.set_utf_8 ppf (Option.value utf_8 ~default:(Colour.utf_8 ~getenv));
  Console.Color.set_depth ppf (Colour.depth ~getenv)

let setup ?style_renderer ?utf_8 () =
  List.iter (set ?style_renderer ?utf_8) outputs

(* Fmt keeps what a program chose for a formatter in tag functions it
   installs on the first choice, so a formatter whose tag functions are still
   Format's own, but for its theme, has had nothing chosen for it. *)
let ensure () =
  List.iter (fun out -> if Console_slot.unset (fst out) then set out) outputs

let is_terminal fd = Unix.isatty fd && not (Colour.dumb ~getenv)
