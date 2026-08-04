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

let rgb r g b =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
    invalid_arg "Console.Color.rgb: component outside 0..255";
  Rgb (r, g, b)

let hex str =
  let str =
    if String.length str > 0 && str.[0] = '#' then
      String.sub str 1 (String.length str - 1)
    else str
  in
  let len = String.length str in
  let hex_digit ch =
    match Ascii.hex_value_int ch with
    | -1 -> invalid_arg "Console.Color.hex: invalid hex character"
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
  | _ -> invalid_arg "Console.Color.hex: expected 3 or 6 hex digits"

let palette index =
  if index < 0 || index > 255 then
    invalid_arg "Console.Color.palette: index outside 0..255";
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

let ansi_position = function
  | `Black | `Bright_black -> 0
  | `Red | `Bright_red -> 1
  | `Green | `Bright_green -> 2
  | `Yellow | `Bright_yellow -> 3
  | `Blue | `Bright_blue -> 4
  | `Magenta | `Bright_magenta -> 5
  | `Cyan | `Bright_cyan -> 6
  | `White | `Bright_white -> 7

let is_bright = function
  | `Bright_black | `Bright_red | `Bright_green | `Bright_yellow | `Bright_blue
  | `Bright_magenta | `Bright_cyan | `Bright_white ->
      true
  | _ -> false

let to_code ~normal ~bright ~extended = function
  | Ansi color ->
      string_of_int
        ((if is_bright color then bright else normal) + ansi_position color)
  | Rgb (r, g, b) -> Fmt.str "%d;2;%d;%d;%d" extended r g b
  | Palette index -> Fmt.str "%d;5;%d" extended index

let to_fg_code = to_code ~normal:30 ~bright:90 ~extended:38
let to_bg_code = to_code ~normal:40 ~bright:100 ~extended:48

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
