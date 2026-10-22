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
  if width < 0 then invalid_arg "Console.Bar.horizontal: negative width";
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  { style; track; brackets; width; frac }

let framed ~glyphs ~track ~brackets body =
  let left, right =
    match glyphs with `Line -> ("\u{2595}", "\u{258f}") | _ -> ("[", "]")
  in
  if brackets then
    View_core.concat
      [ View_core.ink track left; body; View_core.ink track right ]
  else body

(* The fill and the track. Heavy lines go to the half cell, the track
   starting with a right half where a whole filled cell meets it; blocks and
   ASCII fill whole cells. *)
let cells ~glyphs ~width frac =
  let whole fill track =
    let full = int_of_float (Float.round (frac *. float_of_int width)) in
    (Widget.fill full fill, Widget.fill (width - full) track)
  in
  match glyphs with
  | `Ascii -> whole "#" "-"
  | `Block -> whole "\u{2588}" "\u{2591}"
  | `Line ->
      let e = int_of_float (Float.round (frac *. float_of_int (width * 2))) in
      let full = e / 2 and half = e mod 2 = 1 in
      let rest = width - full - Bool.to_int half in
      let fill = Widget.fill full "━" ^ if half then "╸" else "" in
      let track =
        if full > 0 && (not half) && rest > 0 then
          "╺" ^ Widget.fill (rest - 1) "━"
        else Widget.fill rest "━"
      in
      (fill, track)

(* A palette of several colours is spread along the whole bar, each filled
   cell in the colour of its place: the fill uncovers the colours as it
   grows rather than recolouring the cells it has. A palette is never
   empty. *)
let fill ~style ~width look glyphs =
  match (style, Theme.palette look.Look.theme) with
  | Some style, _ -> View_core.ink style glyphs
  | None, [] -> View_core.ink (Widget.accent look) glyphs
  | None, [ c ] -> View_core.ink (Style.fg c) glyphs
  | None, palette ->
      let last = float_of_int (max 1 (width - 1)) in
      View_core.recell
        (fun ~cell ->
          let c = View_core.color_at palette (float_of_int cell /. last) in
          Some (fun style glyph -> (Style.(style + fg c), glyph)))
        (View_core.text glyphs)

let draw t look =
  let track = Option.value t.track ~default:(Widget.muted look) in
  let glyphs = Widget.bar look in
  let filled, rest = cells ~glyphs ~width:t.width t.frac in
  framed ~glyphs ~track ~brackets:t.brackets
    View_core.(fill ~style:t.style ~width:t.width look filled ++ ink track rest)

(* A bar given no width grows to its room, ten cells at its natural size and
   one at least. *)
let horizontal ?style ?track ?brackets ?width frac =
  match width with
  | Some width -> View_core.drawn (draw (v ?style ?track ?brackets ~width frac))
  | None ->
      View_core.deferred ~grow:1 (fun c ->
          let width = match Ctx.width c with Some w -> max 1 w | None -> 10 in
          View_core.drawn (draw (v ?style ?track ?brackets ~width frac)))

(* {1 Vertical bars} *)

let eighths = [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

(* Off UTF-8, a cell is a blank, a dot, a colon or a hash, as it is empty,
   filled to less than half, less than seven eighths, or more. *)
let ascii_eighths = [| " "; "."; "."; "."; ":"; ":"; ":"; "#"; "#" |]

(* The eighths of each cell, from the bottom up: whole cells, then the cell
   the rest fills, then empty ones. *)
let column ~height frac =
  let total = int_of_float (Float.round (frac *. float_of_int (height * 8))) in
  List.init height (fun i -> max 0 (min 8 (total - (i * 8))))

(* Cell [i] from the bottom, filled to [e] eighths, in [look]. *)
let vertical_cell ~style ~track ~height i e look =
  let glyphs = if Widget.bar look = `Ascii then ascii_eighths else eighths in
  if e = 0 then
    View_core.ink (Option.value track ~default:(Widget.muted look)) glyphs.(0)
  else
    let style =
      match (style, Theme.palette look.Look.theme) with
      | Some style, _ -> style
      | None, [] -> Widget.accent look
      | None, [ c ] -> Style.fg c
      | None, palette ->
          let last = float_of_int (max 1 (height - 1)) in
          Style.fg (View_core.color_at palette (float_of_int i /. last))
    in
    View_core.ink style glyphs.(e)

(* A vertical bar is one cell wide and as tall as its room, one row at its
   natural size. *)
let vertical ?style ?track frac =
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  View_core.deferred (fun c ->
      let height = match Ctx.height c with Some h -> max 1 h | None -> 1 in
      View_core.concat ~sep:(View_core.text "\n")
        (List.rev
           (List.mapi
              (fun i e ->
                View_core.drawn (vertical_cell ~style ~track ~height i e))
              (column ~height frac))))
