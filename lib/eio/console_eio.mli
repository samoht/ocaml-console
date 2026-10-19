(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] decides once whether the standard outputs are in colour, and
    runs a {!Console.Display} on the terminal: it finds out whether the output
    is a terminal and how large it is, redraws the progress rows ten times a
    second, prints {!Logs} messages and the program's own output above them, and
    gives the terminal back as it found it when the function returns or raises,
    when its fiber is cancelled, when the program exits and when SIGINT or
    SIGTERM ends it. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val setup : ?style_renderer:Fmt.style_renderer -> unit -> unit
(** [setup ()] decides whether {!Fmt.stdout} and {!Fmt.stderr} are in colour, as
    the OCaml command-line tools do: not when [NO_COLOR] is set and not empty,
    nor when [TERM] is unset, empty or [dumb], and otherwise iff the output is a
    terminal. [style_renderer], when given, is the program's choice and decides
    instead, as [Fmt_tty.setup] takes it: a [--color] option made with
    [Fmt_cli.style_renderer] is passed here as it is.

    It sets {!Fmt.utf_8} on both iff the locale's character set is UTF-8: that
    of the first of [LC_ALL], [LC_CTYPE] and [LANG] that is set and not empty.

    It also sets their {!Console.Color.val-depth}: [`True_color] if [COLORTERM]
    is [truecolor] or [24bit] or [TERM] ends in [-direct], [`Ansi_256] if [TERM]
    contains [256color], and [`Ansi_16] otherwise, so that every widget and span
    printed on them uses colours the terminal has.

    A program calls it once, first; {!run} calls it if it was not. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display] for a display on [ppf] (defaults to
    {!Fmt.stderr}, so that standard output stays the program's), timed by
    [clock]. [header], [theme] and [layout] are those of {!Console.Display.v},
    but for the default theme on a terminal whose formatter is not {!Fmt.utf_8}
    ({!setup} decides it): it is {!Console.Theme.ascii}, so that no character
    the terminal cannot show is drawn.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr}
    and that output is a terminal whose [TERM] is set and not [dumb]; then what
    the program prints on {!Fmt.stdout} or {!Fmt.stderr} in the meantime, a
    whole line at a time, goes above the rows, and so do {!Logs} messages.
    Otherwise each line is printed in turn. When [f] returns, raises or its
    fiber is cancelled, the tasks still running are cancelled, unless [f] said
    the work succeeded with {!Console.Display.set_result}; an exception is
    raised again. Either way, and when the program exits during [f], the display
    closes. So it does when SIGINT or SIGTERM arrives during [f] and the program
    left that signal at its default disposition: the display closes, then the
    signal ends the program as it would have. A signal the program handles or
    ignores is left to it.

    A [run] called inside [f], or in a fiber [f] started, passes its own
    function the same display, so a library can call [run] without knowing
    whether its caller already did.

    @raise Invalid_argument
      if a display opened elsewhere in the program is still running. *)
