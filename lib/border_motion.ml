(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ASCII glyphs the rain flickers through. *)
let rain_glyphs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ:.=*+-<>|/"

(* The glyph of cell [p] in tenth [k] of a second: the same pair always picks
   the same glyph, so a frame is a function of its time. 507224497 is the
   golden-ratio constant 2654435761 reduced mod 2^30, which the 30-bit mask
   makes equivalent and keeps within a 31-bit int. *)
let rain_glyph p k =
  let h = ((p * 507224497) + (k * 40503)) land 0x3fffffff in
  String.make 1 rain_glyphs.[h mod String.length rain_glyphs]

(* How many cells cell [p] is behind a head that runs [speed] cells a
   second round a perimeter of [n] cells. *)
let behind ~n ~speed elapsed p =
  let head = Float.to_int (Float.rem (elapsed *. speed) (Float.of_int n)) in
  (head - p + n) mod n

let recolour c style = Style.(style + fg c)
let bright c style = Style.(style + bold + fg c)

(* A blank glyph is the spacing of a frame without lines: it stays blank. *)
let rain theme ~n ~elapsed p style glyph =
  let d = behind ~n ~speed:15. elapsed p and trail = min 10 (n - 1) in
  let glyph =
    if d < trail && glyph <> " " then
      rain_glyph p (Float.to_int (elapsed *. 10.))
    else glyph
  in
  if d = 0 then (bright (Theme.accent theme) style, glyph)
  else if d <= trail / 2 then (recolour (Theme.accent theme) style, glyph)
  else if d < trail then (recolour (Theme.muted theme) style, glyph)
  else (style, glyph)

(* Two rainbows round the frame, travelling clockwise. *)
let sweep theme ~n ~elapsed p style glyph =
  let palette = Array.of_list (Theme.palette theme) in
  let colours = Array.length palette in
  let band = max 2 (n / (2 * colours)) in
  let shift = Float.to_int (Float.rem (elapsed *. 8.) (Float.of_int n)) in
  let i = (p - shift + n) mod n / band mod colours in
  (recolour palette.(i) style, glyph)

(* A lap takes four seconds, at a speed the eye follows whatever the size. *)
let shimmer theme ~n ~elapsed p style glyph =
  let speed = Float.min 30. (Float.max 8. (Float.of_int n /. 4.)) in
  let d = behind ~n ~speed elapsed p in
  let d = min d (n - d) in
  if d = 0 then (bright (Theme.accent theme) style, glyph)
  else if d <= 2 then (recolour (Theme.accent theme) style, glyph)
  else (style, glyph)

let cell_of = function `Rain -> rain | `Sweep -> sweep | `Shimmer -> shimmer

(* Cells are numbered clockwise from the top-left corner: the top line, the
   right edge, the bottom line from the right and the left edge from the
   bottom. A cell of an edge wider than one has its line's number. *)
let position ~width ~height ~left ~right ~y x =
  if y = 0 then Some x
  else if y = height - 1 then Some (width + height - 2 + (width - 1 - x))
  else if x >= width - right then Some (width + y - 1)
  else if x < left then Some ((2 * width) + height - 2 + (height - 2 - y))
  else None

let frame (look : Look.t) ~left ~right ?(gaps = []) lines =
  match (Theme.motion look.theme, lines) with
  | None, _ | _, ([] | [ _ ]) -> lines
  | Some motion, first :: _ ->
      let width = Span_core.width first and height = List.length lines in
      if
        left = 0 || right = 0 || width < 2
        || List.exists (fun l -> Span_core.width l <> width) lines
      then lines
      else
        let n = (2 * width) + (2 * (height - 2)) in
        let draw = cell_of motion look.theme ~n in
        let gap y x =
          List.exists (fun (g, a, b) -> g = y && a <= x && x < b) gaps
        in
        List.mapi
          (fun y line ->
            let border ~elapsed ~cell =
              if gap y cell then None
              else
                Option.map
                  (fun p -> draw ~elapsed p)
                  (position ~width ~height ~left ~right ~y cell)
            in
            Span_core.drawn (fun (now : Look.t) ->
                match now.elapsed with
                | Some elapsed when Float.is_finite elapsed && elapsed >= 0. ->
                    Span_core.recell (border ~elapsed) line
                | _ -> line))
          lines
