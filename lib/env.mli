(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How a formatter prints, from the environment.

    The rules by which a formatter on an output gets colour, a colour depth and
    UTF-8, from the environment and from whether the output is a terminal, which
    the caller says. They need no Unix: a program that cannot link it asks the
    terminal as it can, such as with [Out_channel.isatty], and the [console.tty]
    library asks [Unix.isatty] for {!Fmt.stdout} and {!Fmt.stderr}
    ([Console_tty.setup]). *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Theme.t ->
  is_tty:bool ->
  Format.formatter ->
  unit
(** [setup ~is_tty ppf] sets the style renderer, the {!Fmt.utf_8}, the
    {!Color.val-depth} and the {!Theme} of [ppf], an output that is a terminal
    iff [is_tty]:

    - The style renderer is [style_renderer] if given, as
      [Fmt_cli.style_renderer] reads a [--color] option. Otherwise it is [`None]
      if [NO_COLOR] is set and not empty, or if [TERM] is unset, empty or
      [dumb]. Else it is [`Ansi_tty] iff [is_tty], or [CONSOLE_CAST] records the
      output. [CLICOLOR_FORCE] and [FORCE_COLOR] are not read: [--color=always]
      forces colour.
    - {!Fmt.utf_8} is [utf_8] if given, else [true] iff the locale's character
      set is UTF-8. The locale is the first of [LC_ALL], [LC_CTYPE] and [LANG]
      that is set and not empty, as POSIX orders them. An unset locale is not
      UTF-8, and the widgets print ASCII alone on it.
    - The colour depth is [`True_color] if [COLORTERM] is [truecolor] or
      [24bit], or if [TERM] ends in [-direct]. It is [`Ansi_256] if [TERM]
      contains [256color], and [`Ansi_16] otherwise.
    - The theme is [theme] if given. Otherwise [ppf] keeps its own. *)

val ensure : is_tty:bool -> Format.formatter -> unit
(** [ensure ~is_tty ppf] is [setup ~is_tty ppf] ({!setup}) if the program chose
    nothing for [ppf], and does nothing otherwise. Such a choice is a style
    renderer or {!Fmt.utf_8}, set with [Fmt], [Fmt_tty] or {!setup}, or a
    {!Color.val-depth}. A {!Theme} is not one. *)
