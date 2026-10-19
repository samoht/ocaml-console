(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Sgr.style = {
  bold : bool;
  faint : bool;
  italic : bool;
  underline : bool;
  reverse : bool;
  strikethrough : bool;
  fg : Color.t option;
  bg : Color.t option;
}

let none =
  {
    bold = false;
    faint = false;
    italic = false;
    underline = false;
    reverse = false;
    strikethrough = false;
    fg = None;
    bg = None;
  }

let bold = { none with bold = true }
let faint = { none with faint = true }
let italic = { none with italic = true }
let underline = { none with underline = true }
let reverse = { none with reverse = true }
let strikethrough = { none with strikethrough = true }
let fg color = { none with fg = Some color }
let bg color = { none with bg = Some color }
let merge_opt left right = match right with Some _ -> right | None -> left

let ( + ) left right =
  {
    bold = left.bold || right.bold;
    faint = left.faint || right.faint;
    italic = left.italic || right.italic;
    underline = left.underline || right.underline;
    reverse = left.reverse || right.reverse;
    strikethrough = left.strikethrough || right.strikethrough;
    fg = merge_opt left.fg right.fg;
    bg = merge_opt left.bg right.bg;
  }

let foreground style = style.fg
let background style = style.bg

let is_none style =
  (not style.bold) && (not style.faint) && (not style.italic)
  && (not style.underline) && (not style.reverse) && (not style.strikethrough)
  && style.fg = None && style.bg = None

let opt_equal eq left right =
  match (left, right) with
  | None, None -> true
  | Some l, Some r -> eq l r
  | _ -> false

let equal left right =
  Bool.equal left.bold right.bold
  && Bool.equal left.faint right.faint
  && Bool.equal left.italic right.italic
  && Bool.equal left.underline right.underline
  && Bool.equal left.reverse right.reverse
  && Bool.equal left.strikethrough right.strikethrough
  && opt_equal Color.equal left.fg right.fg
  && opt_equal Color.equal left.bg right.bg

let pp ppf style =
  if is_none style then Fmt.pf ppf "none"
  else
    let parts = List.map snd (Sgr.attributes style) in
    let parts =
      match style.fg with
      | Some p -> parts @ [ Fmt.str "fg(%a)" Color.pp p ]
      | None -> parts
    in
    let parts =
      match style.bg with
      | Some p -> parts @ [ Fmt.str "bg(%a)" Color.pp p ]
      | None -> parts
    in
    Fmt.pf ppf "%s" (String.concat " + " parts)
