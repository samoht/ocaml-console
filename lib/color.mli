(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal colors.

    Supports ANSI 16-color palette, 256-color palette, and true color (RGB/hex).
*)

(** {1 Color types} *)

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

(** {1 Constructors} *)

val ansi : ansi -> t
(** [ansi c] is the named 16-colour palette colour [c]. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is the 24-bit colour with the given components (each 0 to 255).
    @raise Invalid_argument if a component is out of range. *)

val hex : string -> t
(** [hex s] is the colour parsed from a hex string like ["#RRGGBB"] or
    ["RRGGBB"].
    @raise Invalid_argument if the string is malformed. *)

val palette : int -> t
(** [palette n] is the 256-colour palette entry [n].
    @raise Invalid_argument if [n] is not 0 to 255. *)

(** {1 Predefined colors} *)

val black : t
(** Black. *)

val red : t
(** Red. *)

val green : t
(** Green. *)

val yellow : t
(** Yellow. *)

val blue : t
(** Blue. *)

val magenta : t
(** Magenta. *)

val cyan : t
(** Cyan. *)

val white : t
(** White. *)

val bright_black : t
(** Bright black. *)

val bright_red : t
(** Bright red. *)

val bright_green : t
(** Bright green. *)

val bright_yellow : t
(** Bright yellow. *)

val bright_blue : t
(** Bright blue. *)

val bright_magenta : t
(** Bright magenta. *)

val bright_cyan : t
(** Bright cyan. *)

val bright_white : t
(** Bright white. *)

(** {1 ANSI codes} *)

val to_fg_code : t -> string
(** [to_fg_code c] is the ANSI escape sequence that sets the foreground to [c]
    (without the leading [ESC\[]). *)

val to_bg_code : t -> string
(** [to_bg_code c] is the ANSI escape sequence that sets the background to [c]
    (without the leading [ESC\[]). *)

(** {1 Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same colour. *)

val pp : t Fmt.t
(** [pp] pretty-prints [c]. *)
