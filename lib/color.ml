(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ansi =
  [ `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `Bright_black
  | `Bright_red
  | `Bright_green
  | `Bright_yellow
  | `Bright_blue
  | `Bright_magenta
  | `Bright_cyan
  | `Bright_white ]

type t = Ansi of ansi | Rgb of int * int * int | Palette of int

let ansi color = Ansi color
let clamp v = max 0 (min 255 v)
let rgb r g b = Rgb (clamp r, clamp g, clamp b)

let hex str =
  let str =
    if String.length str > 0 && str.[0] = '#' then
      String.sub str 1 (String.length str - 1)
    else str
  in
  let len = String.length str in
  let hex_digit ch =
    match Ascii.hex_value_int ch with
    | -1 -> invalid_arg "Color.hex: invalid hex character"
    | v -> v
  in
  let parse_hex2 i =
    let hi = str.[i] and lo = str.[i + 1] in
    (hex_digit hi * 16) + hex_digit lo
  in
  let parse_hex1 i =
    let d = hex_digit str.[i] in
    (d * 16) + d
  in
  match len with
  | 6 -> Rgb (parse_hex2 0, parse_hex2 2, parse_hex2 4)
  | 3 -> Rgb (parse_hex1 0, parse_hex1 1, parse_hex1 2)
  | _ -> invalid_arg "Color.hex: expected 3 or 6 hex digits"

let palette index =
  if index < 0 || index > 255 then
    invalid_arg "Color.palette: index out of range";
  Palette index

(* Predefined colors *)
let black = Ansi `Black
let red = Ansi `Red
let green = Ansi `Green
let yellow = Ansi `Yellow
let blue = Ansi `Blue
let magenta = Ansi `Magenta
let cyan = Ansi `Cyan
let white = Ansi `White
let bright_black = Ansi `Bright_black
let bright_red = Ansi `Bright_red
let bright_green = Ansi `Bright_green
let bright_yellow = Ansi `Bright_yellow
let bright_blue = Ansi `Bright_blue
let bright_magenta = Ansi `Bright_magenta
let bright_cyan = Ansi `Bright_cyan
let bright_white = Ansi `Bright_white

let ansi_fg_code = function
  | `Black -> "30"
  | `Red -> "31"
  | `Green -> "32"
  | `Yellow -> "33"
  | `Blue -> "34"
  | `Magenta -> "35"
  | `Cyan -> "36"
  | `White -> "37"
  | `Bright_black -> "90"
  | `Bright_red -> "91"
  | `Bright_green -> "92"
  | `Bright_yellow -> "93"
  | `Bright_blue -> "94"
  | `Bright_magenta -> "95"
  | `Bright_cyan -> "96"
  | `Bright_white -> "97"

let ansi_bg_code = function
  | `Black -> "40"
  | `Red -> "41"
  | `Green -> "42"
  | `Yellow -> "43"
  | `Blue -> "44"
  | `Magenta -> "45"
  | `Cyan -> "46"
  | `White -> "47"
  | `Bright_black -> "100"
  | `Bright_red -> "101"
  | `Bright_green -> "102"
  | `Bright_yellow -> "103"
  | `Bright_blue -> "104"
  | `Bright_magenta -> "105"
  | `Bright_cyan -> "106"
  | `Bright_white -> "107"

let to_fg_code = function
  | Ansi color -> ansi_fg_code color
  | Rgb (r, g, b) -> Fmt.str "38;2;%d;%d;%d" r g b
  | Palette index -> Fmt.str "38;5;%d" index

let to_bg_code = function
  | Ansi color -> ansi_bg_code color
  | Rgb (r, g, b) -> Fmt.str "48;2;%d;%d;%d" r g b
  | Palette index -> Fmt.str "48;5;%d" index

let equal left right =
  match (left, right) with
  | Ansi l, Ansi r -> l = r
  | Rgb (r1, g1, b1), Rgb (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | Palette l, Palette r -> l = r
  | _ -> false

let pp ppf = function
  | Ansi `Black -> Fmt.pf ppf "black"
  | Ansi `Red -> Fmt.pf ppf "red"
  | Ansi `Green -> Fmt.pf ppf "green"
  | Ansi `Yellow -> Fmt.pf ppf "yellow"
  | Ansi `Blue -> Fmt.pf ppf "blue"
  | Ansi `Magenta -> Fmt.pf ppf "magenta"
  | Ansi `Cyan -> Fmt.pf ppf "cyan"
  | Ansi `White -> Fmt.pf ppf "white"
  | Ansi `Bright_black -> Fmt.pf ppf "bright_black"
  | Ansi `Bright_red -> Fmt.pf ppf "bright_red"
  | Ansi `Bright_green -> Fmt.pf ppf "bright_green"
  | Ansi `Bright_yellow -> Fmt.pf ppf "bright_yellow"
  | Ansi `Bright_blue -> Fmt.pf ppf "bright_blue"
  | Ansi `Bright_magenta -> Fmt.pf ppf "bright_magenta"
  | Ansi `Bright_cyan -> Fmt.pf ppf "bright_cyan"
  | Ansi `Bright_white -> Fmt.pf ppf "bright_white"
  | Rgb (r, g, b) -> Fmt.pf ppf "#%02x%02x%02x" r g b
  | Palette index -> Fmt.pf ppf "palette(%d)" index
