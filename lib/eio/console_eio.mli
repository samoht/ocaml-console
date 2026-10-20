(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] runs a {!Console.Display} on the terminal: it finds out
    whether the output is a terminal and how large it is, redraws the progress
    rows ten times a second, prints {!Logs} messages and the program's own
    output above them, and gives the terminal back as it found it when the
    function returns or raises, when its fiber is cancelled, when the program
    exits and when SIGINT or SIGTERM ends it. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?layout:Console.Display.Layout.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display] for a display on [ppf] (defaults to
    {!Fmt.stderr}, so that standard output stays the program's), timed by
    [clock]. [header] and [layout] are those of {!Console.Display.v}, and the
    display is drawn in [ppf]'s theme ({!Console.Theme.get}). It first calls
    {!Console_tty.ensure}, so a standard output on which the program chose
    nothing gets the colours its terminal and environment allow.

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

    While [f] runs, the display prints {!Logs} messages after the mark of their
    level, a message from a source other than {!Logs.default} after the source's
    name in brackets, as in [[http] connected]. The reporter installed before
    [run] is put back when [f] ends.

    A [run] called inside [f], or in a fiber [f] started, passes its own
    function the same display, so a library can call [run] without knowing
    whether its caller already did. Its [ppf], [layout] and [header] are
    ignored: the display keeps those of the outer [run], and closes when the
    outer [f] ends, not the inner one.

    @raise Invalid_argument
      if a display opened elsewhere in the program is still running. *)
