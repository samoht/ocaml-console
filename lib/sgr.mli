(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Escape codes for colours and styles (ECMA-48 8.3.117, SELECT GRAPHIC
    RENDITION). {!Color} and {!Style} make these types abstract. *)

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
(** The type for the 16 named colours. *)

(** The type for colours. A palette index and an RGB component are both in
    \[[0];[255]\]. *)
type color = Ansi of ansi | Rgb of int * int * int | Palette of int

type 'c style = {
  bold : bool;
  faint : bool;
  italic : bool;
  underline : bool;
  reverse : bool;
  strikethrough : bool;
  fg : 'c option;
  bg : 'c option;
}
(** The type for styles, in colours of type ['c]. *)

val attributes : _ style -> (string * string) list
(** [attributes s] is the code and the name of each attribute that [s] sets, in
    field order. *)

val start : color style -> string
(** [start s] is the escape code that selects [s], or [""] if [s] sets nothing.
    The colours are used as given, not reduced to a colour depth. *)

val reset : string
(** [reset] is the escape code that turns every attribute off. *)
