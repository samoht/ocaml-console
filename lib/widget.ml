(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type look = { theme : Theme.t; utf_8 : bool }

let look ppf = { theme = Theme.get ppf; utf_8 = Fmt.utf_8 ppf }
let default_look = { theme = Theme.default; utf_8 = true }

(* A formatter that is not UTF-8 shows ASCII alone: glyphs that are not are
   replaced, all of a part at once so that its pieces still join. *)
let shows look glyphs = look.utf_8 || List.for_all Width.is_ascii glyphs

let border look border =
  let border = Option.value border ~default:(Theme.border look.theme) in
  let c = Border.chars border in
  if
    shows look
      [
        c.top_left;
        c.top;
        c.top_right;
        c.left;
        c.right;
        c.bottom_left;
        c.bottom;
        c.bottom_right;
        c.cross;
        c.top_cross;
        c.bottom_cross;
        c.left_cross;
        c.right_cross;
      ]
  then border
  else Border.with_style (Border.style border) Border.ascii

let guide look guide =
  let guide = Option.value guide ~default:(Theme.guide look.theme) in
  let g = Guide.[ branch guide; last guide; pipe guide; space guide ] in
  if shows look g then guide
  else Guide.with_style (Guide.style guide) Guide.ascii

let glyph look g ~ascii = if shows look [ g ] then g else ascii

let spinner look ~elapsed =
  glyph look
    (Spinner.frame (Theme.spinner look.theme) ~elapsed)
    ~ascii:(Spinner.frame Spinner.ascii ~elapsed)

let mark look m =
  glyph look (Theme.mark look.theme m) ~ascii:(Theme.mark Theme.ascii m)

let ellipsis look = if look.utf_8 then Span_core.ellipsis else "..."
let truncate look w s = Span_core.truncate ~ellipsis:(ellipsis look) w s

let accent look = function
  | Some style -> style
  | None -> Style.fg (Theme.accent look.theme)

let one_line span = Span.concat ~sep:(Span.text " ") (Span_core.lines span)
let cells = Width.string_width
let widest glyphs = List.fold_left (fun n g -> max n (cells g)) 0 glyphs

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
          let w =
            Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:1 glyph
              ~pos:offset ~len
          in
          if (not !full) && !used + w <= rest then begin
            Buffer.add_substring part glyph offset len;
            used := !used + w
          end
          else full := true)
        glyph;
      String.concat "" (List.init (n / gw) (Fun.const glyph))
      ^ Buffer.contents part
      ^ String.make (rest - !used) ' '

let rails g lasts =
  String.concat ""
    (List.rev_map
       (fun last -> if last then Guide.space g else Guide.pipe g)
       lasts)

let branch g = function
  | [] -> ""
  | own :: up -> rails g up ^ if own then Guide.last g else Guide.branch g

let pad_glyph n glyph = glyph ^ String.make (max 0 (n - cells glyph)) ' '
let pad n = Span.text (String.make (max 0 n) ' ')

let pp lines ppf v =
  Span_core.pp_lines ppf
    (lines (look ppf) ~margin:(Format.pp_get_margin ppf ()) v)

let to_string lines ?styled ?(width = max_int) v =
  Span_core.string_of_lines ?styled (lines default_look ~margin:width v)
