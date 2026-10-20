(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type drawn = {
  look : Widget.look;
  border : Border.t;
  title : Span.t option;
  subtitle : Span.t option;
  padding : int;
  width : int option;
  lines : Span.t list;
}

(* A panel is drawn in the look of the formatter it is printed on. *)
type t = Widget.look -> drawn

(* The cells of the frame's left and right columns, each as wide as its
   widest glyph. *)
let columns border =
  let c = Border.chars border in
  ( Widget.widest [ c.top_left; c.left; c.bottom_left ],
    Widget.widest [ c.top_right; c.right; c.bottom_right ] )

(* The cells the border and the padding take on a content line. *)
let frame border padding =
  let left, right = columns border in
  left + right + (2 * padding)

let v ?border ?title ?subtitle ?(padding = 1) ?width content =
  if padding < 0 then invalid_arg "Console.Panel.v: negative padding";
  let least =
    match border with
    | Some border -> frame border padding
    | None -> 2 + (2 * padding)
  in
  (match width with
  | Some width when width < least ->
      invalid_arg "Console.Panel.v: width too small for padding"
  | _ -> ());
  let title = Option.map Widget.one_line title
  and subtitle = Option.map Widget.one_line subtitle in
  let lines = Span_core.lines content in
  fun look ->
    let border = Widget.border look border in
    let title = Option.map (Span.with_style (Theme.header look.theme)) title in
    { look; border; title; subtitle; padding; width; lines }

let natural_width panel =
  let label = function Some span -> Span.width span + 2 | None -> 0 in
  List.fold_left
    (fun acc line -> max acc (Span.width line))
    (max (label panel.title) (label panel.subtitle) - (2 * panel.padding))
    panel.lines

(* The cells a content line has: the fixed width's room, else the content's
   own, capped by what [margin] leaves beside the border and padding. *)
let content_width ~margin panel =
  let frame = frame panel.border panel.padding in
  let room =
    match panel.width with
    | Some total -> total - frame
    | None -> natural_width panel
  in
  max 0 (min room (margin - frame))

(* In a box sized to its content, a line wider than the room wraps at its
   words, hanging under the text after its first word; a box of fixed width
   cuts it instead. *)
let fitted_lines panel cw =
  if cw <= 0 then List.map (fun _ -> Span.empty) panel.lines
  else
    List.concat_map
      (fun line ->
        if Span.width line <= cw then [ line ]
        else if panel.width <> None then [ Widget.truncate panel.look cw line ]
        else Span_core.wrap_hanging cw line)
      panel.lines

let pad = Widget.pad

(* A top or bottom edge, [label] centred in it. *)
let edge look style ~left ~mid ~right inner label =
  let draw n = Span.styled style (Widget.fill n mid) in
  let middle =
    match label with
    | Some label when inner >= 3 ->
        let label = Widget.truncate look (inner - 2) label in
        let w = Span.width label in
        let l = (inner - w - 2) / 2 in
        Span.(
          draw l ++ text " " ++ label ++ text " " ++ draw (inner - w - 2 - l))
    | _ -> draw inner
  in
  Span.(styled style left ++ middle ++ styled style right)

let lines ~margin panel =
  let chars = Border.chars panel.border in
  let style = Border.style panel.border in
  let lw, rw = columns panel.border in
  let l = Widget.pad_glyph lw and r = Widget.pad_glyph rw in
  let cw = content_width ~margin panel in
  let inner = cw + (2 * panel.padding) in
  let has_edge l m r label = label <> None || l ^ m ^ r <> "" in
  let top =
    if has_edge chars.top_left chars.top chars.top_right panel.title then
      [
        edge panel.look style ~left:(l chars.top_left) ~mid:chars.top
          ~right:(r chars.top_right) inner panel.title;
      ]
    else []
  in
  let bottom =
    if has_edge chars.bottom_left chars.bottom chars.bottom_right panel.subtitle
    then
      [
        edge panel.look style ~left:(l chars.bottom_left) ~mid:chars.bottom
          ~right:(r chars.bottom_right) inner panel.subtitle;
      ]
    else []
  in
  let body =
    List.map
      (fun line ->
        Span.(
          styled style (l chars.left)
          ++ pad panel.padding ++ line
          ++ pad (cw - width line + panel.padding)
          ++ styled style (r chars.right)))
      (fitted_lines panel cw)
  in
  (* A margin too narrow for the border itself still bounds every line. *)
  List.map (Widget.truncate panel.look margin) (top @ body @ bottom)

let lines look ~margin t = lines ~margin (t look)
let pp = Widget.pp lines
let to_string = Widget.to_string lines
