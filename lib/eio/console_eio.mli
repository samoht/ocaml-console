(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] runs a {!Console.Display} on the terminal: it finds out
    whether the output is a terminal and how large it is, redraws the progress
    rows ten times a second, passes it the {!Logs} messages, prints the
    program's own output above the rows, and gives the terminal back as it found
    it when the function returns or raises, when its fiber is cancelled, when
    the program exits and when SIGINT or SIGTERM ends it.

    {1:output Where text goes}

    While a display runs, your program's text goes to one of three places. What
    it prints on standard output with [Fmt.pr] is its result: console never
    moves or hides it. On the terminal it only waits for each whole line and
    prints it above the progress rows. Under each running task, console shows
    that task's latest lines (8 unless [~log_lines] says otherwise). Those lines
    are either what you hand it with [Display.log], such as a command's output,
    or the [Logs] messages logged inside the task, meaning in the function given
    to [Display.with_task] or in a fiber that function started. Log messages
    always stay. Command output stays only if the task fails.

    {t
      | What                                   | Terminal, while running | Terminal, once the task ends       | Pipe or CI                                         |
      |----------------------------------------|-------------------------|------------------------------------|----------------------------------------------------|
      | [Fmt.pr] (stdout)                      | above the rows          | stays                              | unchanged, on stdout                               |
      | [Logs] outside a task                  | above the rows          | stays                              | printed at once                                    |
      | [Logs] inside a task                   | in the task's window    | stays above, after the task's name | printed at once, after the task's name             |
      | [Display.log], task succeeds           | in the task's window    | removed                            | printed at once, after the task's name             |
      | [Display.log], task fails              | in the task's window    | all of it printed under the ✗ line | printed at once; the last 8 lines repeated under ✗ |
      | [Display.log], task cancelled (Ctrl-C) | in the task's window    | what was in the window stays       | printed at once                                    |
    }

    Text that [print_string], [Printf] or a child process writes on the terminal
    is not seen and garbles the rows: print it with
    {!Console.Display.with_output}. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?layout:Console.Display.Layout.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display] for a display on [ppf] (defaults to
    {!Fmt.stderr}, so that standard output stays the program's), timed by the
    monotonic [clock], such as [Eio.Stdenv.mono_clock env]: the elapsed times,
    the rates, the spinners and the motion of a scene read it and the redraws
    wait on it, so a change of the system's time moves none of them. A test
    passes an [Eio_mock.Clock.Mono] clock and moves it by hand. [header] and
    [layout] are those of {!Console.Display.v}, and the display is drawn in
    [ppf]'s theme ({!Console.Theme.get}). It first calls {!Console_tty.ensure},
    so a standard output on which the program chose nothing gets the colours its
    terminal and environment allow.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr}
    and that output is a terminal whose [TERM] is set and not [dumb]; then what
    the program prints on {!Fmt.stdout} or {!Fmt.stderr} in the meantime on that
    terminal, a whole line at a time, goes above the rows, and what it prints on
    another output goes out as it is written. Otherwise each line is printed in
    turn. When [f] returns, raises or its fiber is cancelled, the tasks still
    running are cancelled, unless [f] said the work succeeded with
    {!Console.Display.set_result}; an exception is raised again. Either way, and
    when the program exits during [f], the display closes. So it does when
    SIGINT or SIGTERM arrives during [f] and the program left that signal at its
    default disposition: the display closes, then the signal ends the program as
    it would have. A signal the program handles or ignores is left to it.

    While [f] runs, {!Logs} messages go to the display
    ({!Console.Display.report}): to the window of the task current in the
    calling fiber, the one of the innermost {!Console.Display.with_task} of that
    fiber or of the fibers it was forked from, or above the rows when there is
    none. A message from a source other than {!Logs.default} comes after the
    source's name in brackets, as in [[http] connected]. A fiber cancelled
    inside a {!Console.Display.with_task} cancels its task. The reporter
    installed before [run] is put back when [f] ends.

    A [run] called inside [f], or in a fiber [f] started, passes its own
    function the same display, so a library can call [run] without knowing
    whether its caller already did. Its [clock], [ppf], [layout] and [header]
    are ignored: the display keeps those of the outer [run], and closes when the
    outer [f] ends, not the inner one.

    @raise Invalid_argument
      if a display opened elsewhere in the program is still running. *)
