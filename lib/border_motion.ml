(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rain_glyphs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ:.=*+-<>|/"

(* [rain_glyph p k] is the glyph of cell [p] at tenth of a second [k]. It
   depends only on [p] and [k], so a frame depends only on its time. The odd
   multiplier spreads neighbouring cells over the glyphs. The 30-bit mask
   keeps the hash non-negative on a 31-bit int. *)
let rain_glyph p k =
  let h = ((p * 507224497) + (k * 40503)) land 0x3fffffff in
  String.make 1 rain_glyphs.[h mod String.length rain_glyphs]

(* [behind ~n ~speed elapsed p] is how many cells cell [p] is behind a head
   that moves [speed] cells per second round a perimeter of [n] cells. *)
let behind ~n ~speed elapsed p =
  let head = Float.to_int (Float.rem (elapsed *. speed) (Float.of_int n)) in
  (head - p + n) mod n

(* A frame with no theme colour is drawn faint. A cell that the motion
   colours loses the faint attribute, so its colour keeps its contrast. *)
let base style = if Style.equal style Style.faint then Style.none else style
let recolour c style = Style.(base style + fg c)
let bright c style = Style.(base style + bold + fg c)

(* A drop is a bold head in the accent colour, then a trail that fades from
   the accent colour to the muted one. The head and the two cells behind it
   show random glyphs, but only on straight lines. Corners and tees keep
   their glyph, so the frame stays whole. *)
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

(* [graded colours ~n ~elapsed] spreads [colours] once round the frame, from
   the first colour at the top-left corner back to it. The gradient has moved
   clockwise at 8 cells per second for [elapsed] seconds. *)
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

(* A lap takes four seconds. The speed stays between 8 and 30 cells per
   second, so the eye can follow it on a frame of any size. *)
let shimmer theme ~line:_ ~n ~elapsed p style glyph =
  let speed = Float.min 30. (Float.max 8. (Float.of_int n /. 4.)) in
  let d = behind ~n ~speed elapsed p in
  let d = min d (n - d) in
  if d = 0 then (bright (Theme.accent theme) style, glyph)
  else if d <= 2 then (recolour (Theme.accent theme) style, glyph)
  else (style, glyph)

let cell_of = function `Rain -> rain | `Sweep -> sweep | `Shimmer -> shimmer

(* Cells are numbered clockwise from the top-left corner: the top line, the
   right edge, the bottom line from right to left, then the left edge from
   bottom to top. On an edge wider than one cell, the cells of one line share
   a number. *)
let position ~width ~height ~left ~right ~y x =
  if y = 0 then Some x
  else if y = height - 1 then Some (width + height - 2 + (width - 1 - x))
  else if x >= width - right then Some (width + y - 1)
  else if x < left then Some ((2 * width) + height - 2 + (height - 2 - y))
  else None

(* The border's gradient does not move. The theme's motion is drawn over
   it. *)
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
        (* When not live, a sweep is drawn at time 0, because it colours the
           whole frame. Rain and shimmer only highlight a few cells, so they
           are not drawn. *)
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
