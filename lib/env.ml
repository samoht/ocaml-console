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

(* Fmt stores a formatter's style renderer and UTF-8 flag in tag functions,
   which it installs on the first change. If a formatter still has Format's
   own tag functions (apart from its theme slot), nothing was configured. *)
let ensure ~is_tty ppf = if Slot.unset ppf then setup ~is_tty ppf
