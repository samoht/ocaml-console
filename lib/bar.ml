(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  style : Style.t option;
  track : Style.t option;
  brackets : bool;
  width : int;
  frac : float;
}

let v ?style ?track ?(brackets = false) ~width frac =
  if width < 0 then invalid_arg "Console.Bar.v: negative width";
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  { style; track; brackets; width; frac }

(* Eighth-of-a-cell glyphs. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let framed ~utf_8 ~track ~brackets body =
  let left, right = if utf_8 then ("▕", "▏") else ("[", "]") in
  if brackets then
    Span.concat [ Span.styled track left; body; Span.styled track right ]
  else body

(* In ASCII, the fill is whole cells of [#] on a track of [-]. *)
let cells ~utf_8 ~width frac =
  if utf_8 then
    let e = int_of_float (Float.round (frac *. float_of_int (width * 8))) in
    let full = e / 8 and part = e mod 8 in
    let fill =
      Widget.fill full eighths.(8) ^ if part > 0 then eighths.(part) else ""
    in
    (fill, Widget.fill (width - full - Bool.to_int (part > 0)) "░")
  else
    let full = int_of_float (Float.round (frac *. float_of_int width)) in
    (Widget.fill full "#", Widget.fill (width - full) "-")

let span ~utf_8 ~style ~track ~brackets ~width frac =
  let fill, rest = cells ~utf_8 ~width frac in
  framed ~utf_8 ~track ~brackets Span.(styled style fill ++ styled track rest)

let lines look ~margin t =
  let style = Option.value t.style ~default:(Widget.accent look) in
  let track = Option.value t.track ~default:(Widget.muted look) in
  [
    Widget.truncate look margin
      (span ~utf_8:look.utf_8 ~style ~track ~brackets:t.brackets ~width:t.width
         t.frac);
  ]

let pp = Widget.pp lines
let to_string ?styled t = Widget.to_string lines ?styled t
