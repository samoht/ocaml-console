(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour and character set from the environment. *)

val dumb : getenv:(string -> string option) -> bool
(** [dumb ~getenv] is [true] iff [TERM] is unset, empty or [dumb]. A [dumb]
    terminal has no cursor movement. *)

val renderer :
  getenv:(string -> string option) -> is_tty:bool -> Fmt.style_renderer
(** [renderer ~getenv ~is_tty] is [`None] if [NO_COLOR] is set and not empty, or
    if {!dumb} holds. Otherwise it is [`Ansi_tty] iff [is_tty]. *)

val depth :
  getenv:(string -> string option) -> [ `Ansi_16 | `Ansi_256 | `True_color ]
(** [depth ~getenv] is the colour depth the environment announces:
    - [`True_color] if [COLORTERM] is [truecolor] or [24bit], or [TERM] ends in
      [-direct];
    - [`Ansi_256] if [TERM] contains [256color];
    - [`Ansi_16] otherwise. *)

val utf_8 : getenv:(string -> string option) -> bool
(** [utf_8 ~getenv] is [true] iff the locale's character set is UTF-8. The
    locale is the first of [LC_ALL], [LC_CTYPE] and [LANG] that is set and not
    empty. An unset locale is not UTF-8. *)
