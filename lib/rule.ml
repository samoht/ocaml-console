(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type t = { style : Style.t; glyph : string; align : align; label : Span.t }

let v ?theme ?style ?(glyph = "─") ?(align = `Center) ?(label = Span.empty) () =
  let glyph = Console_text.sanitize ~keep_newlines:false glyph in
  if Width.string_width glyph <> 1 then
    invalid_arg "Console.Rule.v: glyph must occupy one cell";
  let style =
    match (style, theme) with
    | Some style, _ -> style
    | None, Some theme -> Style.fg (Theme.accent theme)
    | None, None -> Style.faint
  in
  { style; glyph; align; label = Widget.one_line label }

let space = Span.text " "

let line t width =
  let fill n =
    Span.styled t.style (String.concat "" (List.init n (Fun.const t.glyph)))
  in
  let label = Span.truncate width t.label in
  let available = width - Span.width label in
  if Span.width label = 0 then fill width
  else if available = 0 then label
  else
    match t.align with
    | `Left -> Span.(label ++ space ++ fill (available - 1))
    | `Right -> Span.(fill (available - 1) ++ space ++ label)
    | `Center when available = 1 -> Span.(label ++ space)
    | `Center ->
        let left = (available - 2) / 2 in
        let right = available - 2 - left in
        Span.(fill left ++ space ++ label ++ space ++ fill right)

let pp ppf t = Span.pp ppf (line t (max 0 (Format.pp_get_margin ppf ())))
