(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    Styles can be composed: [Style.(bold + fg Color.red + underline)]. *)

(** {1 Style type} *)

type t
(** A composable style specification. *)

(** {1 Constructors} *)

val none : t
(** No styling. *)

val bold : t
(** Bold text. *)

val faint : t
(** Faint text. *)

val italic : t
(** Italic text. *)

val underline : t
(** Underlined text. *)

val blink : t
(** Blinking text. *)

val reverse : t
(** Reverse video text. *)

val strikethrough : t
(** Strikethrough text. *)

val fg : Color.t -> t
(** Foreground color. *)

val bg : Color.t -> t
(** Background color. *)

(** {1 Composition} *)

val ( + ) : t -> t -> t
(** [s1 + s2] combines two styles. Later styles override earlier ones for
    conflicting attributes. *)

val merge : t list -> t
(** [merge styles] merges a list of styles left to right. *)

(** {1 ANSI escape codes} *)

val to_ansi : t -> string
(** [to_ansi s] is the ANSI escape sequence that enables [s], or the empty
    string for {!none}. *)

val reset : string
(** [reset] is the ANSI escape sequence to reset all styling. *)

(** {1 Fmt integration} *)

val styled : t -> 'a Fmt.t -> 'a Fmt.t
(** [styled style pp] wraps a formatter with ANSI styling.

    Example: [Fmt.pr "%a" (Style.styled Style.bold Fmt.string) "hello"]. *)

val pp_styled : t -> ('a, Format.formatter, unit) format -> 'a
(** [pp_styled style fmt] prints with styling to [Format.std_formatter]. *)

(** {1 Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same style. *)

val pp : t Fmt.t
(** [pp] pretty-prints [s]. *)

val is_none : t -> bool
(** [is_none s] is [true] if [s] has no styling. *)
