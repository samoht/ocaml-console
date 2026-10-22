(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] runs a {!Console.Display} on the terminal. {!run}:

    - finds out whether the output is a terminal, and its size;
    - redraws the progress rows ten times a second;
    - gives the display the program's output and its {!Logs} messages;
    - gives the terminal back as it found it when the run ends, when the program
      exits and when SIGINT or SIGTERM ends it ({{!signals}signals and exits}).

    {1:output Where text goes}

    A task's output shows under it while it runs, and stays if the task fails.

    - Outside a task, output goes above the rows.
    - On a pipe, each line is printed at once, after its task's name.
    - [Display.log] gives a task lines from where it is not current, such as a
      fiber that reads a child process.

    Output is everything the program prints while a task is current:

    - its [Logs] messages ({{!logs}Logs messages});
    - its [Display.log] lines;
    - the lines of {!Fmt.stdout} and {!Fmt.stderr} that {!run} takes
      ({{!Console.Display.output}where text goes}).

    Of what [run] takes, bytes belong to the task current when they are written.
    A task's end flushes {!Fmt.stdout} and {!Fmt.stderr}, so bytes that another
    fiber printed there without flushing them go to whichever task ends first.
    Each task collects its own lines, so a line is what one task wrote up to a
    line feed, and a task's unended last line is ended when the task ends. Bytes
    written outside any task are the output of no task, ended when the display
    closes.

    Text that [print_string], [Printf] or a child process writes on the terminal
    goes past the display and garbles the rows, so print it with
    {!Console.Display.with_output}. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?layout:Console.Display.Layout.t ->
  ?summary:string ->
  ?view:Console.Display.view ->
  ?keep:bool ->
  ?progress:Console.Progress.t ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display], for a display that closes when [f] ends.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]. The
      elapsed times, the rates, the spinners and the motion of a view read it,
      and the redraws wait on it, so a change of the system's time moves none of
      them. A test passes an [Eio_mock.Clock.Mono] clock and moves it by hand.
    - [ppf] is where the display draws (defaults to {!Fmt.stderr}, so that
      standard output stays the program's). The display is drawn in [ppf]'s
      theme ({!Console.Theme.get}).
    - [summary], [view], [keep] and [layout] are those of {!Console.Display.v}.
    - [progress] says how progress shows ({!Console.Progress}, defaults to
      [`Auto]). With [`Tty] the rows take the size of the terminal behind the
      output, if there is one, else [$COLUMNS] by [$LINES], each when it is
      between [1] and [4096], else 80 by 24.

    [run] first calls {!Console_tty.ensure}, so that a standard output on which
    the program chose nothing gets the colours its terminal and environment
    allow.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr},
    that output is a terminal whose [TERM] is set and not [dumb], and [progress]
    does not ask for lines. What the program then prints on {!Fmt.stdout} or
    {!Fmt.stderr} on that terminal is taken a whole line at a time ({!output}).
    A line longer than 64 KiB is taken in pieces as it is written, each cut
    between two characters and, unless an escape code holds the whole piece,
    outside an escape code. What it prints on another output goes out as it is
    written.

    - With [`Tty] on another output, what the program prints on [ppf] itself is
      taken in the same way.
    - With [`Quiet] or [`Json], what the program prints on {!Fmt.stdout} or
      {!Fmt.stderr}, when it writes where [ppf] does, as with [2>&1], is taken
      in the same way. With [`Quiet] the output of a task that succeeds is not
      printed, and with [`Json] each line is an [output] event, so that every
      line of [ppf] is an event.

    When [f] returns, raises or its fiber is cancelled, the tasks still running
    are cancelled, unless [f] said the work succeeded with
    {!Console.Display.set_result}. The display closes either way, and an
    exception from [f] is raised again. A fiber cancelled inside a
    {!Console.Display.with_task} cancels its task.

    The sections below say what [run] does with {{!logs}Logs messages},
    {{!signals}signals and exits}, {{!nested}nested runs} and a
    {{!deterministic}deterministic clock}.

    @raise Invalid_argument if a display runs in another domain. *)

(** {1:running Running a display}

    {2:logs Logs messages}

    While [f] runs, {!Logs} messages go to the display.

    - A message is the output of the task current in the calling fiber: the
      innermost {!Console.Display.with_task} of that fiber or of the fibers it
      was forked from. With no such task, it goes above the rows.
    - A message from a source other than {!Logs.default} comes after the
      source's name in brackets, as in [[http] connected].
    - A message logged in another domain that Eio runs, such as one of an
      [Eio.Executor_pool], is passed to the domain that runs [f]. A worker does
      not inherit the task of the fiber that submitted its work, so the message
      goes above the rows unless the worker made a task current with
      {!Console.Display.within}. The display itself belongs to that domain alone
      ({{!Console.Display.displays}Displays}).

    A message the display cannot take goes to the reporter installed before
    [run], so that a log call never fails. Such a message is one of:
    - a message logged after the display closed, through a reporter kept from
      [f];
    - a message still waiting for the display when it closes;
    - a message from a domain that Eio does not run.

    That reporter is put back when [f] ends, and when the program exits during
    [f], so what an [at_exit] function logs after the display closed goes to it.

    {2:signals Signals and exits}

    The display closes when the program exits during [f], and signals left at
    their default disposition are handled as follows.

    - SIGINT or SIGTERM during [f]: the display closes, then the signal ends the
      program as it would have.
    - SIGTSTP (Ctrl-Z): the rows are taken off the terminal and the cursor is
      shown before the program stops, and the rows are drawn again once [fg]
      continues it.

    A signal the program handles or ignores is left to it.

    A closed pipe on [ppf] ends the display's output and nothing else, while one
    on standard output is met by the program's own writes, as without console
    ({!Console.Display.v}).

    {2:nested Nested runs}

    A [run] called while another runs in the same domain passes its own function
    the same display. This holds inside [f], in a fiber that [f] started and in
    a fiber beside it, so a library can call [run] without knowing whether its
    caller already did.

    - The [clock], [ppf], [layout], [summary], [view], [keep] and [progress] of
      the inner [run] are ignored, since the display keeps those of the first
      [run].
    - The display closes once the last of their functions ends, and the first
      [run] waits for the others.
    - A [run] that starts while that display closes opens its own once it is
      gone. So does one from a fiber that outlived the display it was started
      under, unless another display runs then, which it joins.

    {2:deterministic A deterministic clock}

    [CONSOLE_DETERMINISTIC_CLOCK], set in the environment to a value other than
    the empty string, makes the time the display reads depend on the program
    alone, so that the progress a test captures from the program reads the same
    on every machine. On a clock that {!clock} made, the time moves by exactly
    the duration of each sleep on it. On any other clock it stays at zero:
    - every elapsed time reads [0.0s], and every [time] and [elapsed] of a
      {{!Console.Progress.json}JSON event} reads [0];
    - no rate shows;
    - the spinners and the motion stand still.

    The redraws still wait on the clock behind it, and do not move its time. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is [c], or, under {{!deterministic}[CONSOLE_DETERMINISTIC_CLOCK]},
    a clock that reads the time its latest sleep ended at. A sleep of [d] on it
    waits on [c] until [d] after the time it read when the sleep started, and
    then reads that time, so the elapsed times, the rates and the spinners of a
    display that {!run} draws on it follow the program's sleeps. *)
