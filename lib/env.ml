(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let getenv = Sys.getenv_opt

let setup ?style_renderer ?utf_8 ?theme ~is_tty ppf =
  let renderer =
    match style_renderer with
    | Some r -> r
    | None ->
        let is_tty = is_tty || Option.is_some (Cast.file ()) in
        Colour.renderer ~getenv ~is_tty
  in
  Fmt.set_style_renderer ppf renderer;
  Fmt.set_utf_8 ppf (Option.value utf_8 ~default:(Colour.utf_8 ~getenv));
  Color.set_depth ppf (Colour.depth ~getenv);
  Option.iter (Theme.set ppf) theme

(* Fmt keeps what a program chose for a formatter in tag functions it
   installs on the first choice, so a formatter whose tag functions are still
   Format's own, but for its theme, has had nothing chosen for it. *)
let ensure ~is_tty ppf = if Slot.unset ppf then setup ~is_tty ppf
