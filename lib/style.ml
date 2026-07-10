(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  bold : bool option;
  faint : bool option;
  italic : bool option;
  underline : bool option;
  blink : bool option;
  reverse : bool option;
  strikethrough : bool option;
  fg : Color.t option;
  bg : Color.t option;
}

let none =
  {
    bold = None;
    faint = None;
    italic = None;
    underline = None;
    blink = None;
    reverse = None;
    strikethrough = None;
    fg = None;
    bg = None;
  }

let bold = { none with bold = Some true }
let faint = { none with faint = Some true }
let italic = { none with italic = Some true }
let underline = { none with underline = Some true }
let blink = { none with blink = Some true }
let reverse = { none with reverse = Some true }
let strikethrough = { none with strikethrough = Some true }
let fg color = { none with fg = Some color }
let bg color = { none with bg = Some color }
let merge_opt left right = match right with Some _ -> right | None -> left

let ( + ) left right =
  {
    bold = merge_opt left.bold right.bold;
    faint = merge_opt left.faint right.faint;
    italic = merge_opt left.italic right.italic;
    underline = merge_opt left.underline right.underline;
    blink = merge_opt left.blink right.blink;
    reverse = merge_opt left.reverse right.reverse;
    strikethrough = merge_opt left.strikethrough right.strikethrough;
    fg = merge_opt left.fg right.fg;
    bg = merge_opt left.bg right.bg;
  }

let merge = List.fold_left ( + ) none

(* Active boolean attributes paired with their ANSI code and human name.
   [bool_attrs] and [bool_attr_names] are filters over this list. *)
let active_bool_attrs style =
  let pick flag code name =
    match flag with Some true -> Some (code, name) | _ -> None
  in
  List.filter_map
    (fun x -> x)
    [
      pick style.bold "1" "bold";
      pick style.faint "2" "faint";
      pick style.italic "3" "italic";
      pick style.underline "4" "underline";
      pick style.blink "5" "blink";
      pick style.reverse "7" "reverse";
      pick style.strikethrough "9" "strikethrough";
    ]

let bool_attrs style = List.map fst (active_bool_attrs style)

let to_ansi style =
  let codes = bool_attrs style in
  let codes =
    match style.fg with
    | Some c -> codes @ [ Color.to_fg_code c ]
    | None -> codes
  in
  let codes =
    match style.bg with
    | Some c -> codes @ [ Color.to_bg_code c ]
    | None -> codes
  in
  match codes with
  | [] -> ""
  | _ -> Fmt.str "\027[%sm" (String.concat ";" codes)

let reset = "\027[0m"

let styled style pp ppf value =
  let ansi = to_ansi style in
  if ansi = "" then pp ppf value
  else (
    Fmt.pf ppf "%s" ansi;
    pp ppf value;
    Fmt.pf ppf "%s" reset)

let pp_styled style fmt =
  let ansi = to_ansi style in
  if ansi = "" then Fmt.pr fmt
  else (
    Fmt.pf Fmt.stdout "%s" ansi;
    Fmt.kpf (fun ppf -> Fmt.pf ppf "%s" reset) Fmt.stdout fmt)

let is_none style =
  style.bold = None && style.faint = None && style.italic = None
  && style.underline = None && style.blink = None && style.reverse = None
  && style.strikethrough = None && style.fg = None && style.bg = None

let opt_equal eq left right =
  match (left, right) with
  | None, None -> true
  | Some l, Some r -> eq l r
  | _ -> false

let equal left right =
  opt_equal ( = ) left.bold right.bold
  && opt_equal ( = ) left.faint right.faint
  && opt_equal ( = ) left.italic right.italic
  && opt_equal ( = ) left.underline right.underline
  && opt_equal ( = ) left.blink right.blink
  && opt_equal ( = ) left.reverse right.reverse
  && opt_equal ( = ) left.strikethrough right.strikethrough
  && opt_equal Color.equal left.fg right.fg
  && opt_equal Color.equal left.bg right.bg

let bool_attr_names style = List.map snd (active_bool_attrs style)

let pp ppf style =
  if is_none style then Fmt.pf ppf "none"
  else
    let parts = bool_attr_names style in
    let parts =
      match style.fg with
      | Some c -> parts @ [ Fmt.str "fg(%a)" Color.pp c ]
      | None -> parts
    in
    let parts =
      match style.bg with
      | Some c -> parts @ [ Fmt.str "bg(%a)" Color.pp c ]
      | None -> parts
    in
    Fmt.pf ppf "%s" (String.concat " + " parts)
