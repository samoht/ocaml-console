(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  border : Border.t;
  title : Span.t option;
  subtitle : Span.t option;
  padding : int;
  width : int option;
  lines : Span.t list;
}

let cells s = Width.string_width (Console_text.sanitize ~keep_newlines:false s)

(* The cells the border and the padding take on a content line. *)
let frame border padding =
  let chars = Border.chars border in
  cells chars.left + cells chars.right + (2 * padding)

let v ?theme ?border ?title ?subtitle ?(padding = 1) ?width content =
  let border = Widget.border ?theme ?border ~default:Border.rounded () in
  if padding < 0 then invalid_arg "Console.Panel.v: negative padding";
  (match width with
  | Some width when width < frame border padding ->
      invalid_arg "Console.Panel.v: width too small for padding"
  | _ -> ());
  let title = Option.map Widget.one_line title
  and subtitle = Option.map Widget.one_line subtitle in
  { border; title; subtitle; padding; width; lines = Span.lines content }

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
        else if panel.width <> None then [ Span.truncate cw line ]
        else Span.wrap_hanging cw line)
      panel.lines

let pad = Widget.pad

(* A top or bottom edge, [label] centred in it. *)
let edge style ~left ~mid ~right inner label =
  let draw n = Span.styled style (Widget.repeat n mid) in
  let middle =
    match label with
    | Some label when inner >= 3 ->
        let label = Span.truncate (inner - 2) label in
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
  let cw = content_width ~margin panel in
  let inner = cw + (2 * panel.padding) in
  let has_edge l m r label = label <> None || l ^ m ^ r <> "" in
  let top =
    if has_edge chars.top_left chars.top chars.top_right panel.title then
      [
        edge style ~left:chars.top_left ~mid:chars.top ~right:chars.top_right
          inner panel.title;
      ]
    else []
  in
  let bottom =
    if has_edge chars.bottom_left chars.bottom chars.bottom_right panel.subtitle
    then
      [
        edge style ~left:chars.bottom_left ~mid:chars.bottom
          ~right:chars.bottom_right inner panel.subtitle;
      ]
    else []
  in
  let body =
    List.map
      (fun line ->
        Span.(
          styled style chars.left ++ pad panel.padding ++ line
          ++ pad (cw - width line + panel.padding)
          ++ styled style chars.right))
      (fitted_lines panel cw)
  in
  (* A margin too narrow for the border itself still bounds every line. *)
  List.map (Span.truncate margin) (top @ body @ bottom)

let pp = Widget.pp lines
let to_string = Widget.to_string lines
