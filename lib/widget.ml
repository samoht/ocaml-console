(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let border ?(theme = Theme.unicode) ?border () =
  match border with Some border -> border | None -> Theme.border theme

let one_line span = Span.concat ~sep:(Span.text " ") (Span_core.lines span)
let cells = Width.string_width

let fill n glyph =
  if n <= 0 then ""
  else
    let gw = cells glyph in
    if gw = 0 then String.make n ' '
    else
      let rest = n mod gw in
      let part = Buffer.create rest and used = ref 0 and full = ref false in
      Glyph.String.iter_graphemes
        (fun ~offset ~len ->
          let w = cells (String.sub glyph offset len) in
          if (not !full) && !used + w <= rest then begin
            Buffer.add_substring part glyph offset len;
            used := !used + w
          end
          else full := true)
        glyph;
      String.concat "" (List.init (n / gw) (Fun.const glyph))
      ^ Buffer.contents part
      ^ String.make (rest - !used) ' '

let pad_glyph n glyph = glyph ^ String.make (max 0 (n - cells glyph)) ' '
let pad n = Span.text (String.make (max 0 n) ' ')

let pp lines ppf v =
  Span_core.pp_lines ppf (lines ~margin:(Format.pp_get_margin ppf ()) v)

let to_string lines ?styled v =
  Span_core.string_of_lines ?styled (lines ~margin:max_int v)
