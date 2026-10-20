(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Sgr

type t = Color.t Sgr.style

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

let equal left right =
  Bool.equal left.bold right.bold
  && Bool.equal left.faint right.faint
  && Bool.equal left.italic right.italic
  && Bool.equal left.underline right.underline
  && Bool.equal left.reverse right.reverse
  && Bool.equal left.strikethrough right.strikethrough
  && Option.equal Color.equal left.fg right.fg
  && Option.equal Color.equal left.bg right.bg

let is_none style = equal style none

(* The style the enclosing [styled] calls of a formatter have on, which an
   inner call adds to and selects again when it ends. *)
let current = Console_slot.v ~setting:false ()

let styled style pp_v ppf v =
  match Fmt.style_renderer ppf with
  | `None -> pp_v ppf v
  | `Ansi_tty when is_none style -> pp_v ppf v
  | `Ansi_tty ->
      let outer = Option.value (Console_slot.find current ppf) ~default:none in
      let inner = outer + style and depth = Color.depth ppf in
      let code s = Format.pp_print_as ppf 0 s in
      Console_slot.set current ppf inner;
      code (Escape.sgr depth inner);
      Fun.protect
        ~finally:(fun () ->
          Console_slot.set current ppf outer;
          code Sgr.reset;
          code (Escape.sgr depth outer))
        (fun () -> pp_v ppf v)

let pp ppf style =
  if is_none style then Fmt.pf ppf "none"
  else
    let colour name c =
      Option.to_list (Option.map (Fmt.str "%s(%a)" name Color.pp) c)
    in
    Fmt.(list ~sep:(any " + ") string)
      ppf
      (List.map snd (attributes style)
      @ colour "fg" style.fg @ colour "bg" style.bg)
