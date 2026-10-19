(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  cross : string;
  top_cross : string;
  bottom_cross : string;
  left_cross : string;
  right_cross : string;
}

type t = chars Inked.t

let v = Inked.v
let chars = Inked.get
let style = Inked.style

let none_chars =
  {
    top_left = "";
    top = "";
    top_right = "";
    left = "";
    right = "";
    bottom_left = "";
    bottom = "";
    bottom_right = "";
    cross = "";
    top_cross = "";
    bottom_cross = "";
    left_cross = "";
    right_cross = "";
  }

let ascii_chars =
  {
    top_left = "+";
    top = "-";
    top_right = "+";
    left = "|";
    right = "|";
    bottom_left = "+";
    bottom = "-";
    bottom_right = "+";
    cross = "+";
    top_cross = "+";
    bottom_cross = "+";
    left_cross = "+";
    right_cross = "+";
  }

let single_chars =
  {
    top_left = "┌";
    top = "─";
    top_right = "┐";
    left = "│";
    right = "│";
    bottom_left = "└";
    bottom = "─";
    bottom_right = "┘";
    cross = "┼";
    top_cross = "┬";
    bottom_cross = "┴";
    left_cross = "├";
    right_cross = "┤";
  }

let double_chars =
  {
    top_left = "╔";
    top = "═";
    top_right = "╗";
    left = "║";
    right = "║";
    bottom_left = "╚";
    bottom = "═";
    bottom_right = "╝";
    cross = "╬";
    top_cross = "╦";
    bottom_cross = "╩";
    left_cross = "╠";
    right_cross = "╣";
  }

let rounded_chars =
  {
    top_left = "╭";
    top = "─";
    top_right = "╮";
    left = "│";
    right = "│";
    bottom_left = "╰";
    bottom = "─";
    bottom_right = "╯";
    cross = "┼";
    top_cross = "┬";
    bottom_cross = "┴";
    left_cross = "├";
    right_cross = "┤";
  }

let heavy_chars =
  {
    top_left = "┏";
    top = "━";
    top_right = "┓";
    left = "┃";
    right = "┃";
    bottom_left = "┗";
    bottom = "━";
    bottom_right = "┛";
    cross = "╋";
    top_cross = "┳";
    bottom_cross = "┻";
    left_cross = "┣";
    right_cross = "┫";
  }

let hidden_chars =
  {
    top_left = " ";
    top = " ";
    top_right = " ";
    left = " ";
    right = " ";
    bottom_left = " ";
    bottom = " ";
    bottom_right = " ";
    cross = " ";
    top_cross = " ";
    bottom_cross = " ";
    left_cross = " ";
    right_cross = " ";
  }

let none = v none_chars
let ascii = v ascii_chars
let single = v single_chars
let double = v double_chars
let rounded = v rounded_chars
let heavy = v heavy_chars
let hidden = v hidden_chars
let with_style = Inked.with_style

let pp ppf t =
  let chars = Inked.get t in
  Fmt.pf ppf "%s%s%s@." chars.top_left chars.top chars.top_right;
  Fmt.pf ppf "%s %s@." chars.left chars.right;
  Fmt.pf ppf "%s%s%s" chars.bottom_left chars.bottom chars.bottom_right
