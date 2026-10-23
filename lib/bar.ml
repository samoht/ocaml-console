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

(* A cell's colour is that of the height its fill reaches: up the theme's
   palette if it has several colours, and otherwise from the colour of a
   success through that of a warning to that of a failure, so that the
   cells of a sparkline show their values. *)
let height_style ~style look p =
  match style with
  | Some style -> style
  | None ->
      let theme = look.Look.theme in
      let stops =
        match Theme.palette theme with
        | [] | [ _ ] ->
            [ Theme.succeeded theme; Theme.warning theme; Theme.failed theme ]
        | palette -> palette
      in
      Style.fg (View_core.color_at stops p)

(* Cell [i] from the bottom of [height], filled to [e] eighths, [width]
   cells wide, in [look]. *)
let vertical_cell ~style ~track ~height ~width i e look =
  let glyphs = if Widget.bar look = `Ascii then ascii_eighths else eighths in
  let glyph = Widget.fill width glyphs.(e) in
  if e = 0 then
    View_core.ink (Option.value track ~default:(Widget.muted look)) glyph
  else
    let p = (float_of_int i +. (float_of_int e /. 8.)) /. float_of_int height in
    View_core.ink (height_style ~style look p) glyph

let clamp frac =
  if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)

let draw_vertical ~style ~track ~width ~height frac =
  View_core.concat ~sep:(View_core.text "\n")
    (List.rev
       (List.mapi
          (fun i e ->
            View_core.drawn (vertical_cell ~style ~track ~height ~width i e))
          (column ~height frac)))

(* A vertical bar is as tall as its room, one row at its natural size, and
   as wide as it is told, or as its room in a row that shares it. *)
let vertical ?style ?track ?width frac =
  (match width with
  | Some w when w < 0 -> invalid_arg "Console.Bar.vertical: negative width"
  | _ -> ());
  let frac = clamp frac in
  View_core.deferred
    ~grow:(if width = None then 1 else 0)
    (fun c ->
      let height = match Ctx.height c with Some h -> max 1 h | None -> 1 in
      let width =
        match (width, Ctx.width c) with
        | Some w, _ -> w
        | None, Some w -> max 1 w
        | None, None -> 1
      in
      draw_vertical ~style ~track ~width ~height frac)

(* {1 Charts} *)

(* A sparkline spans its values: the lowest is the least a cell shows, an
   eighth, unless [min] says lower, and the highest fills it. It is one row
   in a room of any height. *)
let sparkline ?style ?min ?max values =
  let lo = List.fold_left Float.min Float.infinity values
  and hi = List.fold_left Float.max Float.neg_infinity values in
  let hi = Option.value max ~default:hi in
  let lo, floor = match min with Some m -> (m, 0.) | None -> (lo, 1. /. 8.) in
  let frac v =
    if hi <= lo then if v > lo then 1. else floor
    else floor +. ((1. -. floor) *. ((v -. lo) /. (hi -. lo)))
  in
  let cell v =
    let e = int_of_float (Float.round (clamp (frac v) *. 8.)) in
    View_core.drawn (vertical_cell ~style ~track:None ~height:1 ~width:1 0 e)
  in
  View_core.concat (List.map cell values)

let value v =
  if Float.is_integer v && Float.abs v < 1e15 then Fmt.str "%.0f" v
  else Fmt.str "%.3g" v

let chart ?height ?scale ?style bars =
  let top =
    match scale with
    | Some s -> s
    | None -> List.fold_left (fun m (_, v) -> Float.max m v) 0. bars
  in
  let frac v = if top > 0. then v /. top else 0. in
  View_core.deferred (fun c ->
      let look = Ctx.look c in
      let rows =
        match (height, Ctx.height c) with
        | Some h, _ -> max 1 h
        | None, Some h -> max 1 (h - 3)
        | None, None -> 8
      in
      let columns =
        List.map
          (fun (label, v) ->
            let label = View_core.text label in
            let shown = View_core.ink (Widget.muted look) (value v) in
            let w =
              max 1 (max (View_core.width label) (View_core.width shown))
            in
            let at v = View_core.place ~align:`Center ~width:w v in
            (at shown, vertical ?style ~width:w (frac v), at label, w))
          bars
      in
      let band f = View_core.hcat ~gap:1 (List.map f columns) in
      let total =
        List.fold_left (fun n (_, _, _, w) -> n + w + 1) (-1) columns
      in
      let base =
        View_core.ink (Widget.muted look)
          (Widget.fill (max 0 total) (Widget.glyph look "─" ~ascii:"-"))
      in
      View_core.concat ~sep:(View_core.text "\n")
        [
          band (fun (v, _, _, _) -> v);
          View_core.resolve
            (Ctx.with_room ~width:None ~height:(Some rows) c)
            (band (fun (_, b, _, _) -> b));
          base;
          band (fun (_, _, l, _) -> l);
        ])
