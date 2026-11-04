(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rain_glyphs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ:.=*+-<>|/"

(* The glyph of cell [p] in tenth [k] of a second. The same pair always
   picks the same glyph, so a frame is a function of its time. The odd
   multiplier scatters neighbouring cells over the glyphs, and the 30-bit
   mask keeps the hash non-negative on a 31-bit int. *)
let rain_glyph p k =
  let h = ((p * 507224497) + (k * 40503)) land 0x3fffffff in
  String.make 1 rain_glyphs.[h mod String.length rain_glyphs]

(* How many cells cell [p] is behind a head that runs [speed] cells a
   second round a perimeter of [n] cells. *)
let behind ~n ~speed elapsed p =
  let head = Float.to_int (Float.rem (elapsed *. speed) (Float.of_int n)) in
  (head - p + n) mod n

(* A frame with no colour of the theme is drawn faint. A cell that motion
   colours drops the faint attribute, so that its colour keeps its
   contrast. *)
let base style = if Style.equal style Style.faint then Style.none else style
let recolour c style = Style.(base style + fg c)
let bright c style = Style.(base style + bold + fg c)

(* A drop is a bold head in the main colour followed by a trail that fades
   from the main colour to the muted one. The head and the two cells behind
   it show random glyphs, but only on a straight line: a corner or a tee,
   where lines meet, keeps its glyph so that the frame stays whole. *)
let rain theme ~line ~n ~elapsed p style glyph =
  let d = behind ~n ~speed:15. elapsed p in
  let fade = min 12 (n - 1) in
  let glyph =
    if d < 3 && line glyph then rain_glyph p (Float.to_int (elapsed *. 10.))
    else glyph
  in
  if d = 0 then (bright (Theme.accent theme) style, glyph)
  else if d < fade then
    let t = Float.of_int d /. Float.of_int fade in
    let muted =
      Option.value (Theme.muted theme) ~default:(Theme.accent theme)
    in
    (recolour (Color.blend (Theme.accent theme) muted t) style, glyph)
  else (style, glyph)

(* [colours] spread once round the frame, from the first at the top-left
   corner back to it, moved clockwise at 8 cells a second for [elapsed]
   seconds. *)
let graded colours ~n ~elapsed p style glyph =
  let palette = Array.of_list colours in
  let k = Array.length palette and n = Float.of_int n in
  let at = Float.rem (Float.of_int p -. (elapsed *. 8.)) n in
  let x = (if at < 0. then at +. n else at) /. n *. Float.of_int k in
  let i = min (k - 1) (Float.to_int x) in
  let f = x -. Float.of_int i in
  let c =
    if f = 0. then palette.(i)
    else Color.blend palette.(i) palette.((i + 1) mod k) f
  in
  (recolour c style, glyph)

let sweep theme ~line:_ ~n ~elapsed p style glyph =
  graded (Theme.palette theme) ~n ~elapsed p style glyph

(* A lap takes four seconds, at a speed kept between 8 and 30 cells a second
   so that the eye can follow it on any size of frame. *)
let shimmer theme ~line:_ ~n ~elapsed p style glyph =
  let speed = Float.min 30. (Float.max 8. (Float.of_int n /. 4.)) in
  let d = behind ~n ~speed elapsed p in
  let d = min d (n - d) in
  if d = 0 then (bright (Theme.accent theme) style, glyph)
  else if d <= 2 then (recolour (Theme.accent theme) style, glyph)
  else (style, glyph)

let cell_of = function `Rain -> rain | `Sweep -> sweep | `Shimmer -> shimmer

(* Cells are numbered clockwise from the top-left corner: the top line, the
   right edge, the bottom line from the right and the left edge from the
   bottom. All the cells of one line of an edge wider than one cell share
   its number. *)
let position ~width ~height ~left ~right ~y x =
  if y = 0 then Some x
  else if y = height - 1 then Some (width + height - 2 + (width - 1 - x))
  else if x >= width - right then Some (width + y - 1)
  else if x < left then Some ((2 * width) + height - 2 + (height - 2 - y))
  else None

(* A border's gradient is still; the theme's motion draws over it. *)
let frame (look : Look.t) ~time border ~left ~right ?(gaps = []) lines =
  let motion = Theme.motion look.theme and colours = Border.gradient border in
  match lines with
  | [] | [ _ ] -> lines
  | _ when Option.is_none motion && colours = [] -> lines
  | first :: _ ->
      let width = View_core.width first and height = List.length lines in
      if
        left = 0 || right = 0 || width < 2
        || List.exists (fun l -> View_core.width l <> width) lines
      then lines
      else
        let n = (2 * width) + (2 * (height - 2)) in
        let c = Border.chars border in
        let line g =
          g <> " " && List.mem g [ c.top; c.bottom; c.left; c.right ]
        in
        let still =
          if colours = [] then None else Some (graded colours ~n ~elapsed:0.)
        in
        (* When not live, a sweep keeps its colours at time 0, since they
           cover the whole frame. Rain and a shimmer only highlight a few
           cells, so they are not drawn. *)
        let moving =
          Option.bind motion (fun motion ->
              let elapsed =
                if look.live && Float.is_finite time && time >= 0. then
                  Some time
                else match motion with `Sweep -> Some 0. | _ -> None
              in
              Option.map
                (fun elapsed -> cell_of motion look.theme ~line ~n ~elapsed)
                elapsed)
        in
        let draw p style glyph =
          let style, glyph =
            match still with Some g -> g p style glyph | None -> (style, glyph)
          in
          match moving with Some m -> m p style glyph | None -> (style, glyph)
        in
        let gap y x =
          List.exists (fun (g, a, b) -> g = y && a <= x && x < b) gaps
        in
        let border y ~cell =
          if gap y cell then None
          else
            Option.map
              (fun p -> draw p)
              (position ~width ~height ~left ~right ~y cell)
        in
        if Option.is_none still && Option.is_none moving then lines
        else List.mapi (fun y line -> View_core.recell (border y) line) lines
