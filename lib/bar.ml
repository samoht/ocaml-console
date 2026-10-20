(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  style : Style.t option;
  track : Style.t;
  brackets : bool;
  width : int;
  frac : float;
}

let v ?style ?(track = Style.faint) ?(brackets = false) ~width frac =
  if width < 0 then invalid_arg "Console.Bar.v: negative width";
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  { style; track; brackets; width; frac }

(* Eighth-of-a-cell glyphs. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let framed ~track ~brackets body =
  if brackets then
    Span.concat [ Span.styled track "▕"; body; Span.styled track "▏" ]
  else body

let span ~style ~track ~brackets ~width frac =
  let e = int_of_float (Float.round (frac *. float_of_int (width * 8))) in
  let full = e / 8 and part = e mod 8 in
  let fill =
    Widget.fill full eighths.(8) ^ if part > 0 then eighths.(part) else ""
  in
  let used = full + if part > 0 then 1 else 0 in
  let track_cells = Widget.fill (width - used) "░" in
  framed ~track ~brackets Span.(styled style fill ++ styled track track_cells)

let lines look ~margin t =
  let style = Widget.accent look t.style in
  [
    Span.truncate margin
      (span ~style ~track:t.track ~brackets:t.brackets ~width:t.width t.frac);
  ]

let pp = Widget.pp lines
let to_string ?styled t = Widget.to_string lines ?styled t
