(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    Styles can be composed: [Style.(bold + fg Color.red + underline)]. *)

(** {1:style_type Style type} *)

type t
(** A composable style specification. *)

(** {1:constructors Constructors} *)

val none : t
(** [none] is the empty style. *)

val bold : t
(** [bold] is the style of bold text. *)

val faint : t
(** [faint] is the style of faint text. *)

val italic : t
(** [italic] is the style of italic text. *)

val underline : t
(** [underline] is the style of underlined text. *)

val blink : t
(** [blink] is the style of blinking text. *)

val reverse : t
(** [reverse] is the style of reverse video text. *)

val strikethrough : t
(** [strikethrough] is the style of struck-through text. *)

val fg : Color.t -> t
(** [fg c] is the style with foreground colour [c]. *)

val bg : Color.t -> t
(** [bg c] is the style with background colour [c]. *)

(** {1:composition Composition} *)

val ( + ) : t -> t -> t
(** [s1 + s2] combines two styles. Later styles override earlier ones for
    conflicting attributes. *)

val merge : t list -> t
(** [merge styles] merges a list of styles left to right. *)

(** {1:ansi_escape_codes ANSI escape codes} *)

val to_ansi : t -> string
(** [to_ansi s] is the ANSI escape sequence that enables [s], or the empty
    string for {!none}. *)

val reset : string
(** [reset] is the ANSI escape sequence to reset all styling. *)

(** {1:fmt_integration Fmt integration} *)

val styled : t -> 'a Fmt.t -> 'a Fmt.t
(** [styled style pp] applies [style] when the target formatter's style renderer
    permits ANSI, and otherwise renders with [pp] unchanged.

    Example: [Fmt.pr "%a" (Style.styled Style.bold Fmt.string) "hello"]. *)

(** {1:operations Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same style. *)

val pp : t Fmt.t
(** [pp] pretty-prints [s]. *)

val is_none : t -> bool
(** [is_none s] is [true] if [s] has no styling. *)
