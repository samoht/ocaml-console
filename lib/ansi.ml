(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type color =
  [ `Cyan
  | `Green
  | `Blue
  | `Magenta
  | `Yellow
  | `Red
  | `White
  | `Hex of int * int * int ]

let dim = "\027[2m"
let bold = "\027[1m"
let reset_code = "\027[0m"

let color_code = function
  | `Cyan -> "\027[36m"
  | `Green -> "\027[32m"
  | `Blue -> "\027[34m"
  | `Magenta -> "\027[35m"
  | `Yellow -> "\027[33m"
  | `Red -> "\027[31m"
  | `White -> "\027[37m"
  | `Hex (r, g, b) -> Fmt.str "\027[38;2;%d;%d;%dm" r g b

let styled code s = String.concat "" [ code; s; reset_code ]
let dimmed s = if s = "" then "" else styled dim s
