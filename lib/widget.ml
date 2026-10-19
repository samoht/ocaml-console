(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let border ?theme ?border ~default () =
  match border with
  | Some border -> border
  | None -> Option.fold ~none:default ~some:Theme.border theme

let one_line span = Span.concat ~sep:(Span.text " ") (Span.lines span)

let repeat n glyph =
  if n <= 0 || glyph = "" then ""
  else String.concat "" (List.init n (Fun.const glyph))

let pad n = Span.text (String.make (max 0 n) ' ')

let pp lines ppf v =
  Span.pp_lines ppf (lines ~margin:(Format.pp_get_margin ppf ()) v)

let to_string lines ?styled v =
  Span.string_of_lines ?styled (lines ~margin:max_int v)
