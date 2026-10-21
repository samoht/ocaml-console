(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] runs a {!Console.Display} on the terminal: it finds out
    whether the output is a terminal and how large it is, redraws the progress
    rows ten times a second, gives it the program's output and {!Logs} messages,
    and gives the terminal back as it found it when the function returns or
    raises, when its fiber is cancelled, when the program exits and when SIGINT
    or SIGTERM ends it.

    {1:output Where text goes}

    A task's output shows under it while it runs, and stays if the task fails.

    - Outside a task, output goes above the rows.
    - On a pipe, each line is printed at once, after its task's name.
    - [Display.log] gives a task lines from where it is not current, such as a
      fiber that reads a child process.

    Output is everything the program prints while a task is current: its [Logs]
    messages, its [Display.log] lines and the lines of {!Fmt.stdout} and
    {!Fmt.stderr} on the display's terminal
    ({{!Console.Display.output}where text goes}). Text that [print_string],
    [Printf] or a child process writes on the terminal is not seen and garbles
    the rows: print it with {!Console.Display.with_output}. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

type progress =
  [ `Auto
    (** Rows redrawn in place on a terminal ({!run}), lines in turn otherwise.
    *)
  | `Tty
    (** Rows redrawn in place even on an output not found to be a terminal, such
        as a pipe that a terminal recorder reads, as wide and as tall as the
        terminal behind it if there is one, else [$COLUMNS] by [$LINES], else 80
        by 24. *)
  | `Plain
    (** Lines in turn, as on a pipe: a line when a task starts, its output at
        once, a line when it ends, then the header; in colour if the output has
        colour. *)
  | `Quiet
    (** No rows, no task lines and no header: a task that fails or is cancelled
        prints its line, then its output under it, as on a terminal ({!output}),
        and what goes above the rows, the result of
        {!Console.Display.set_result} included, is printed as usual. *)
  | `Json
    (** One JSON object on each line for each event, and nothing else
        ({{!json}JSON events}). *) ]
(** The type for how a display shows progress, as the [--progress] option of
    [docker build] does. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?layout:Console.Display.Layout.t ->
  ?header:string ->
  ?progress:progress ->
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

    [progress] (defaults to [`Auto]) says how progress shows ({!type-progress}).
    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr}
    and that output is a terminal whose [TERM] is set and not [dumb], unless
    [progress] asks for lines; then what the program prints on {!Fmt.stdout} or
    {!Fmt.stderr} in the meantime on that terminal, a whole line at a time, is
    the output of the current task, or goes above the rows as it is outside a
    task ({!output}), and what it prints on another output goes out as it is
    written. With [`Tty] on another output, the same holds of what the program
    prints on [ppf] itself. With [`Json], what the program prints on [ppf], when
    it is {!Fmt.stdout} or {!Fmt.stderr}, is [output] events, so that each line
    of [ppf] is an event. When [f] returns, raises or its fiber is cancelled,
    the tasks still running are cancelled, unless [f] said the work succeeded
    with {!Console.Display.set_result}; an exception is raised again. Either
    way, and when the program exits during [f], the display closes. So it does
    when SIGINT or SIGTERM arrives during [f] and the program left that signal
    at its default disposition: the display closes, then the signal ends the
    program as it would have. When SIGTSTP (Ctrl-Z) stops the program and it
    left that signal at its default disposition, the rows are taken off the
    terminal and the cursor shown first, and drawn again once [fg] continues it.
    A signal the program handles or ignores is left to it. A closed pipe on
    [ppf] ends the display's output and nothing else, and one on standard output
    is met by the program's own writes, as without console
    ({!Console.Display.v}).

    While [f] runs, {!Logs} messages go to the display: they are the output of
    the task current in the calling fiber, the one of the innermost
    {!Console.Display.with_task} of that fiber or of the fibers it was forked
    from, or go above the rows when there is none. A message from a source other
    than {!Logs.default} comes after the source's name in brackets, as in
    [[http] connected]. A fiber cancelled inside a {!Console.Display.with_task}
    cancels its task. The reporter installed before [run] is put back when [f]
    ends, and when the program exits during [f], so that what an [at_exit]
    function logs after the display closed goes to it. A message logged in
    another domain that Eio runs, such as one of an [Eio.Executor_pool], is
    passed to the domain that runs [f] and goes above the rows; the display
    itself is for that domain alone ({{!Console.Display.displays}Displays}). A
    message the display cannot take goes to the reporter installed before [run],
    so that a log call never fails: one logged once the display closed, through
    a reporter kept from [f], one still waiting for the display when it closes,
    and one from a domain that Eio does not run.

    A [run] called while another runs in the same domain, inside [f], in a fiber
    [f] started or in a fiber beside it, passes its own function the same
    display, so a library can call [run] without knowing whether its caller
    already did. Its [clock], [ppf], [layout], [header] and [progress] are
    ignored: the display keeps those of the first [run], and closes once the
    last of their functions ends; the first [run] waits for the others. A [run]
    that starts while that display closes opens its own once it is gone, and so
    does one from a fiber that outlived the display it was started under, unless
    another display runs then, which it joins.

    [CONSOLE_FROZEN_CLOCK], set in the environment to a value other than the
    empty string, stops the display's clock at zero: every elapsed time reads
    [0.0s], no rate shows, and the spinners and the motion stand still, while
    the redraws still wait on [clock]. The progress a test captures from the
    program then reads the same on every machine.

    @raise Invalid_argument if a display runs in another domain. *)

val progress : (string * progress) list
(** [progress] is the choices of a [--progress] option, as
    [Cmdliner.Arg.enum progress] reads them, for the [progress] argument of
    {!run}:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

(** {1:json JSON events}

    With [`Json] the display writes one JSON object on each line of [ppf] for
    each event, in the order they happen. Objects may gain fields and events may
    be added; the fields below keep their meaning. Every object has:
    - [time], the seconds since the display started, a number with three
      decimals: [0] under [CONSOLE_FROZEN_CLOCK];
    - [event], one of the strings below.

    An event of a task also has [id], an integer that names the task in the
    display, and [task], the labels from the top task down to it, an array of
    strings.
    - ["start"]: the task started; [parent] is the [id] of the task it is under,
      if any.
    - ["progress"]: its count changed; [count] and [total] are integers and
      [unit] is ["items"] or ["bytes"].
    - ["label"]: its label changed to [text].
    - ["output"]: [text] is a line of its output; [level] is that of a [Logs]
      message: ["app"], ["error"], ["warning"], ["info"] or ["debug"]. The
      output of no task has [task] at [[]] and no [id].
    - ["end"]: it ended; [status] is ["succeeded"], ["failed"] or ["cancelled"],
      [elapsed] the seconds it ran and [text] the message it ended with, if any.
    - ["result"]: the result of {!Console.Display.set_result}, when the display
      closes: [status] is ["ok"] or ["error"], [text] its message and [detail]
      its detail, if any.

    Strings are valid UTF-8 and hold no control character but escaped ones. The
    task of {!Console.Display} is these events:
    {v
    {"time":0.000,"event":"start","id":0,"task":["compile"]}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0.000,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0.000,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0.000}
    v} *)
