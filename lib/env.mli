(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Formatter setup from the environment.

    [Env] sets a formatter's colour, colour depth and UTF-8 flag from the
    environment. The caller says whether the output is a terminal. [Env] needs
    no Unix. A program that cannot link Unix can detect a terminal another way,
    such as with [Out_channel.isatty]. The [console.tty] library uses
    [Unix.isatty] for {!Fmt.stdout} and {!Fmt.stderr} ([Console_tty.setup]). *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Theme.t ->
  is_tty:bool ->
  Format.formatter ->
  unit
(** [setup ~is_tty ppf] sets the style renderer, the {!Fmt.utf_8} flag, the
    {!Color.val-depth} and the {!Theme} of [ppf]. [is_tty] says whether [ppf]
    writes to a terminal.

    - The style renderer is [style_renderer] if given, for example as
      [Fmt_cli.style_renderer] reads it from a [--color] option. Otherwise it is
      [`None] if [NO_COLOR] is set and not empty, or if [TERM] is unset, empty
      or [dumb]. Otherwise it is [`Ansi_tty] iff [is_tty] holds or
      [CONSOLE_CAST] records the output. [CLICOLOR_FORCE] and [FORCE_COLOR] are
      not read; use [--color=always] to force colour.
    - {!Fmt.utf_8} is [utf_8] if given. Otherwise it is [true] iff the locale's
      character set is UTF-8. The locale is the first of [LC_ALL], [LC_CTYPE]
      and [LANG] that is set and not empty, in POSIX order. An unset locale is
      not UTF-8, and the widgets then print only ASCII.
    - The colour depth is [`True_color] if [COLORTERM] is [truecolor] or
      [24bit], or if [TERM] ends in [-direct]. It is [`Ansi_256] if [TERM]
      contains [256color], and [`Ansi_16] otherwise.
    - The theme is [theme] if given. Otherwise [ppf] keeps its theme. *)

val ensure : is_tty:bool -> Format.formatter -> unit
(** [ensure ~is_tty ppf] is [setup ~is_tty ppf] ({!setup}) if the program has
    not configured [ppf], and does nothing otherwise. [ppf] is configured if it
    has a style renderer or a {!Fmt.utf_8} flag set (with [Fmt], [Fmt_tty] or
    {!setup}), or a {!Color.val-depth}. Setting a {!Theme} does not count. *)
