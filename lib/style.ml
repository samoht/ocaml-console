(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
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

(* Active boolean attributes paired with their ANSI code and human name.
   [bool_attrs] and [bool_attr_names] are filters over this list. *)
let active_bool_attrs style =
  let pick flag code name = if flag then Some (code, name) else None in
  List.filter_map
    (fun x -> x)
    [
      pick style.bold "1" "bold";
      pick style.faint "2" "faint";
      pick style.italic "3" "italic";
      pick style.underline "4" "underline";
      pick style.reverse "7" "reverse";
      pick style.strikethrough "9" "strikethrough";
    ]

let bool_attrs style = List.map fst (active_bool_attrs style)
let foreground style = style.fg
let background style = style.bg

let to_ansi ?(depth = `True_color) style =
  let colour code c = code (Color.downsample depth c) in
  let codes =
    bool_attrs style
    @ Option.to_list (Option.map (colour Color.to_fg_code) style.fg)
    @ Option.to_list (Option.map (colour Color.to_bg_code) style.bg)
  in
  match codes with
  | [] -> ""
  | _ -> String.concat "" [ "\027["; String.concat ";" codes; "m" ]

let reset = "\027[0m"

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

let bool_attr_names style = List.map snd (active_bool_attrs style)

let pp ppf style =
  if is_none style then Fmt.pf ppf "none"
  else
    let parts = bool_attr_names style in
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
