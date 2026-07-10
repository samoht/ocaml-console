(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Low-level ANSI SGR primitives: a foreground colour and the escape sequences
    that switch graphic attributes on. The styling the live display and {!Bar}
    render with; {!Style} and {!Color} are the richer, user-facing palette. *)

type color =
  [ `Cyan
  | `Green
  | `Blue
  | `Magenta
  | `Yellow
  | `Red
  | `White
  | `Hex of int * int * int ]
(** A foreground colour, named or 24-bit [`Hex (r, g, b)]. *)

val color_code : color -> string
(** [color_code c] is the SGR sequence that sets the foreground to [c]. *)

val dim : string
(** The SGR sequence that starts dim (faint) text. *)

val bold : string
(** The SGR sequence that starts bold text. *)

val reset_code : string
(** The SGR reset sequence. *)

val styled : string -> string -> string
(** [styled code s] wraps [s] in [code] and a {!reset_code}. *)

val dimmed : string -> string
(** [dimmed s] renders [s] dim, or [""] unchanged. *)
