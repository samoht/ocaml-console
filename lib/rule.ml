(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  style : Style.t option;
  glyph : string;
  align : View_core.align;
  label : View_core.t;
}

let v ?style ?(glyph = "─") ?(align = `Center) ?(label = View_core.empty) () =
  let glyph = Console_text.sanitize ~keep_newlines:false glyph in
  if Width.string_width glyph <> 1 then
    invalid_arg "Console.Rule.v: glyph must occupy one cell";
  { style; glyph; align; label = Widget.one_line label }

let space = View_core.text " "

let line look t width =
  let style = Option.value t.style ~default:(Widget.muted look) in
  let glyph = Widget.glyph look t.glyph ~ascii:"-" in
  let fill n = View_core.ink style (Widget.fill n glyph) in
  let label = Widget.truncate look width t.label in
  let available = width - View_core.width label in
  if View_core.width label = 0 then fill width
  else if available = 0 then label
  else
    match t.align with
    | `Left -> View_core.(label ++ space ++ fill (available - 1))
    | `Right -> View_core.(fill (available - 1) ++ space ++ label)
    | `Center when available = 1 -> View_core.(label ++ space)
    | `Center ->
        let left = (available - 2) / 2 in
        let right = available - 2 - left in
        View_core.(fill left ++ space ++ label ++ space ++ fill right)

let pp ppf t =
  View_core.pp ppf
    (line (Widget.look ppf) t (max 0 (Format.pp_get_margin ppf ())))

let lines look ~margin t = [ line look t (max 0 margin) ]
let to_span ~width t = Widget.to_span lines ~width t
let to_string ?styled ~width t = Widget.to_string lines ?styled ~width t
