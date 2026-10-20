(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Eighth-of-a-cell glyphs. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
let default_style = Style.fg (Theme.accent Theme.unicode)

let framed ~track ~brackets body =
  if brackets then
    Span.concat [ Span.styled track "▕"; body; Span.styled track "▏" ]
  else body

let v ?(style = default_style) ?(track = Style.faint) ?(brackets = false) ~width
    frac =
  if width < 0 then invalid_arg "Console.Bar.v: negative width";
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  let e = int_of_float (Float.round (frac *. float_of_int (width * 8))) in
  let full = e / 8 and part = e mod 8 in
  let fill =
    Widget.fill full eighths.(8) ^ if part > 0 then eighths.(part) else ""
  in
  let used = full + if part > 0 then 1 else 0 in
  let track_cells = Widget.fill (width - used) "░" in
  framed ~track ~brackets Span.(styled style fill ++ styled track track_cells)
