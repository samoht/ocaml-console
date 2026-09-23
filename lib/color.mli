(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal colors.

    Supports ANSI 16-color palette, 256-color palette, and true color (RGB/hex).
*)

(** {1:color_types Color types} *)

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
(** Standard 16-color ANSI palette. *)

type t
(** A terminal color. *)

(** {1:constructors Constructors} *)

val ansi : ansi -> t
(** [ansi c] is the named 16-colour palette colour [c]. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is the 24-bit colour with the given components (each 0 to 255).
    {b Raises.} [Invalid_argument] if a component is out of range. *)

val hex : string -> t
(** [hex s] is the colour parsed from a hex string like ["#RRGGBB"] or
    ["RRGGBB"]. {b Raises.} [Invalid_argument] if the string is malformed. *)

val palette : int -> t
(** [palette n] is the 256-colour palette entry [n]. {b Raises.}
    [Invalid_argument] if [n] is not 0 to 255. *)

(** {1:predefined_colors Predefined colors} *)

val black : t
(** [black] is black. *)

val red : t
(** [red] is red. *)

val green : t
(** [green] is green. *)

val yellow : t
(** [yellow] is yellow. *)

val blue : t
(** [blue] is blue. *)

val magenta : t
(** [magenta] is magenta. *)

val cyan : t
(** [cyan] is cyan. *)

val white : t
(** [white] is white. *)

val bright_black : t
(** [bright_black] is bright black. *)

val bright_red : t
(** [bright_red] is bright red. *)

val bright_green : t
(** [bright_green] is bright green. *)

val bright_yellow : t
(** [bright_yellow] is bright yellow. *)

val bright_blue : t
(** [bright_blue] is bright blue. *)

val bright_magenta : t
(** [bright_magenta] is bright magenta. *)

val bright_cyan : t
(** [bright_cyan] is bright cyan. *)

val bright_white : t
(** [bright_white] is bright white. *)

(** {1:ansi_codes ANSI codes} *)

val to_fg_code : t -> string
(** [to_fg_code c] is the ANSI escape sequence that sets the foreground to [c]
    (without the leading [ESC\[]). *)

val to_bg_code : t -> string
(** [to_bg_code c] is the ANSI escape sequence that sets the background to [c]
    (without the leading [ESC\[]). *)

(** {1:operations Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same colour. *)

val pp : t Fmt.t
(** [pp] pretty-prints [c]. *)
