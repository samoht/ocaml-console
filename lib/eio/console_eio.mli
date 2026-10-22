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
    - the lines of {!Fmt.stdout} and {!Fmt.stderr} on the display's terminal
      ({{!Console.Display.output}where text goes}).

    Text that [print_string], [Printf] or a child process writes on the terminal
    goes past the display and garbles the rows, so print it with
    {!Console.Display.with_output}. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

type progress =
  [ `Auto
    (** The rows are redrawn in place on a terminal ({!run}), and lines are
        printed in turn otherwise. *)
  | `Tty
    (** The rows are redrawn in place on any output, such as a pipe that a
        terminal recorder reads. The rows take the size of the terminal behind
        the output, if there is one, else [$COLUMNS] by [$LINES], else 80 by 24.
    *)
  | `Plain
    (** Lines are printed in turn, as on a pipe. A task prints a line when it
        starts, its output at once and a line when it ends. The header comes
        last. The lines are in colour if the output has colour. *)
  | `Quiet
    (** Only what fails is printed. There are no rows, no task lines and no
        header. A task that fails or is cancelled prints its line, then its
        output under it, as on a terminal ({!output}). What goes above the rows
        is printed as usual, the result of {!Console.Display.set_result}
        included. *)
  | `Json
    (** Each event is one JSON object on a line of its own, and nothing else is
        printed ({{!json}JSON events}). *) ]
(** The type for how a display shows progress, as the [--progress] option of
    [docker build] does. Constructors may be added, so a match on a progress has
    a case for the others. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?layout:Console.Display.Layout.t ->
  ?header:string ->
  ?progress:progress ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display], for a display that closes when [f] ends.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]. The
      elapsed times, the rates, the spinners and the motion of a scene read it,
      and the redraws wait on it, so a change of the system's time moves none of
      them. A test passes an [Eio_mock.Clock.Mono] clock and moves it by hand.
    - [ppf] is where the display draws (defaults to {!Fmt.stderr}, so that
      standard output stays the program's). The display is drawn in [ppf]'s
      theme ({!Console.Theme.get}).
    - [header] and [layout] are those of {!Console.Display.v}.
    - [progress] says how progress shows (defaults to [`Auto]).

    [run] first calls {!Console_tty.ensure}, so that a standard output on which
    the program chose nothing gets the colours its terminal and environment
    allow.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr},
    that output is a terminal whose [TERM] is set and not [dumb], and [progress]
    does not ask for lines. What the program then prints on {!Fmt.stdout} or
    {!Fmt.stderr} on that terminal is taken a whole line at a time ({!output}).
    What it prints on another output goes out as it is written.

    - With [`Tty] on another output, what the program prints on [ppf] itself is
      taken in the same way.
    - With [`Quiet], what the program prints on {!Fmt.stdout} or {!Fmt.stderr},
      when it writes where [ppf] does, is taken in the same way, so that the
      output of a task that succeeds is not printed.
    - With [`Json], what the program prints on [ppf], when it is {!Fmt.stdout}
      or {!Fmt.stderr}, becomes [output] events, so that each line of [ppf] is
      an event.

    When [f] returns, raises or its fiber is cancelled, the tasks still running
    are cancelled, unless [f] said the work succeeded with
    {!Console.Display.set_result}. The display closes either way, and an
    exception from [f] is raised again. A fiber cancelled inside a
    {!Console.Display.with_task} cancels its task.

    The sections below say what [run] does with {{!logs}Logs messages},
    {{!signals}signals and exits}, {{!nested}nested runs} and a
    {{!frozen}frozen clock}.

    @raise Invalid_argument if a display runs in another domain. *)

val progress : (string * progress) list
(** [progress] is the choices of a [--progress] option, as
    [Cmdliner.Arg.enum progress] reads them, for the [progress] argument of
    {!run}:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

(** {1:json JSON events}

    With [`Json] the display writes one JSON object on each line of [ppf] for
    each event, in the order the events happen. Objects may gain fields, new
    events may be added, and a field whose values are listed below may gain
    values, but the fields below keep their meaning. A consumer skips an event
    or a value it does not know. The header of the display is not an event.

    Every object has:
    - [time], the seconds since the display started, a number with three
      decimals ([0.000] under [CONSOLE_FROZEN_CLOCK]);
    - [event], one of the strings below.

    An event of a task also has:
    - [id], an integer that names the task in the display: the tasks are
      numbered from [0] in the order they start, and no two tasks of a display
      have the same number;
    - [task], the labels from the top task down to it, an array of strings.

    The events are:
    - ["start"]: the task started. [parent] is the [id] of the task it is under,
      if any.
    - ["progress"]: its count changed. [count] and [total] are integers, and
      [unit] is ["items"] or ["bytes"].
    - ["label"]: its label changed to [text].
    - ["output"]: [text] is a line of its output. [level], only for a [Logs]
      message, is that message's level: ["app"], ["error"], ["warning"],
      ["info"] or ["debug"]. Output outside any task has [task] at [[]] and no
      [id].
    - ["end"]: it ended. [status] is ["succeeded"], ["failed"] or ["cancelled"],
      [elapsed] is the seconds it ran, with three decimals, and [text] is the
      message it ended with, if any.
    - ["result"]: the result of {!Console.Display.set_result}, when the display
      closes. [status] is ["ok"] or ["error"], [text] is its message and
      [detail] its detail, if any.

    A transient task ({!Console.Display.task}) has its events as any other task
    has. Strings are valid UTF-8. A control character in them is always escaped.
    The task in the example of {!Console.Display} gives these events:
    {v
    {"time":0.000,"event":"start","id":0,"task":["compile"]}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0.000,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0.000,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0.000}
    v} *)

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

    - The [clock], [ppf], [layout], [header] and [progress] of the inner [run]
      are ignored, since the display keeps those of the first [run].
    - The display closes once the last of their functions ends, and the first
      [run] waits for the others.
    - A [run] that starts while that display closes opens its own once it is
      gone. So does one from a fiber that outlived the display it was started
      under, unless another display runs then, which it joins.

    {2:frozen A frozen clock}

    [CONSOLE_FROZEN_CLOCK], set in the environment to a value other than the
    empty string, stops the display's clock at zero:
    - every elapsed time reads [0.0s];
    - no rate shows;
    - the spinners and the motion stand still.

    The redraws still wait on [clock], and the progress that a test captures
    from the program reads the same on every machine. *)
