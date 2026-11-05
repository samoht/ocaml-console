(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] shows the tasks of a program ({!Console.Task}) on the
    terminal. {!run}:

    - makes the root of the work and passes it to the program;
    - finds out whether the output is a terminal, and its size, and shows the
      tasks as {!Console.Progress.show} does;
    - redraws the progress rows ten times a second;
    - gives the current task the program's output and its {!Logs} messages;
    - gives the terminal back as it found it when the run ends, when the program
      exits and when SIGINT or SIGTERM ends it ({{!signals}signals and exits}).

    {1:output Where text goes}

    A task's output shows under it while it runs, and stays if the task fails.

    - Outside a task, output goes above the rows.
    - On a pipe, each line is printed at once, after its task's path
      ({!Console.Progress.design}).
    - [Task.log] gives a task lines from where it is not current, such as a
      fiber that reads a child process.

    Output is everything the program prints while a task is current:

    - its [Logs] messages ({{!logs}Logs messages});
    - its [Task.log] lines;
    - the lines of {!Fmt.stdout} and {!Fmt.stderr} that {!run} takes.

    Each byte that [run] takes belongs to the task current when it is written,
    in the fiber that writes it; outside any fiber, as in a thread, no task is
    current. A task's end flushes {!Fmt.stdout} and {!Fmt.stderr}, so bytes that
    another fiber printed there without flushing them go to whichever task ends
    first. Each task collects its own lines, so a line is what one task wrote up
    to a line feed, and a task's unended last line is ended when the task ends.
    Bytes written outside any task belong to no task, and their unended last
    line is ended when the display closes.

    Text that [print_string], [Printf] or a child process writes on the terminal
    goes past the display and garbles the rows, so print it with {!with_output}.
*)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?progress:Console.Progress.t ->
  ?layout:Console.Progress.Layout.t ->
  ?label:string ->
  ?view:(Console.Task.t -> Console.View.t) ->
  ?keep_ended:bool ->
  ?commit:(Console.Task.t -> Console.View.t option) ->
  ?plain:(Console.Task.Event.t -> Console.View.t option) ->
  ?delay:float ->
  (Console.Task.t -> 'a) ->
  'a
(** [run ~clock f] is [f root], where [root] is the root task of the work, shown
    on [ppf] until [f] ends. [root] behaves as a task of {!Console.Task.run}:
    unless [f] ended it ({!Console.Task.succeed}), it succeeds when [f] returns,
    fails when [f] raises and is cancelled when its fiber is. When [root] ends,
    so do the tasks still running.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]. The
      elapsed times, the rates, the spinners and the motion of a view read it,
      and the redraws wait on it, so a change of the system's time moves none of
      them. A test passes an [Eio_mock.Clock.Mono] clock and moves it by hand.
    - [ppf] is where the tasks show (defaults to {!Fmt.stderr}, so that standard
      output stays the program's), in [ppf]'s theme ({!Console.Theme.get}).
    - [label] names the work on the summary line ({!Console.Task.root}).
    - [view] is applied to the root, and [view root] is the [view] of
      {!Console.Progress.show}; [keep_ended], [commit], [plain], [delay] and
      [layout] are [show]'s too.
    - [progress] says how progress shows ({!Console.Progress}, defaults to
      [`Auto]). With [`Tty], the rows are as wide as [$COLUMNS] and as high as
      [$LINES], each when it is between [1] and [4096], since POSIX lets them
      override the size of the terminal; else the size of the terminal behind
      the output; else 80 by 24. When the tasks print as lines on an output that
      is not a terminal, the text above them and the last frame are as wide as
      [$COLUMNS] when it is between [1] and [4096], and not cut otherwise, as
      {!Console.Progress.show} prints them with and without a size.

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

    The display closes once the work has ended, and an exception from [f] is
    raised again. A run whose fiber is cancelled still closes the display and
    restores the terminal, waiting a second at most for an output that takes
    nothing; a run that is not cancelled waits for its output as any write does.
    A fiber cancelled inside a {!Console.Task.run} cancels its task. A [view]
    that raises is replaced by the default one, and [run] raises its exception:
    at once, cancelling [f], when a redraw raises it, and otherwise once the
    display has closed, unless [f] raised first.

    The sections below say what [run] does with {{!logs}Logs messages},
    {{!domains}other domains}, {{!signals}signals and exits},
    {{!nested}nested runs} and a {{!deterministic}deterministic clock}. *)

(** {1:running Running a display}

    {2:logs Logs messages}

    While [f] runs, {!Logs} messages go to the display.

    - A message is the output of the task current in the calling fiber: the
      innermost {!Console.Task.run} or {!Console.Task.within} of that fiber or
      of the fibers it was forked from. With no such task, it goes above the
      rows.
    - A message at the [App] level is printed as it was written, with no header,
      as [Logs_fmt.reporter] prints it: it is the program's own output. A
      message at another level from a source other than {!Logs.default} comes
      after the source's name in brackets, as in [[http] connected].
    - A message logged in another domain, such as one of an [Eio.Executor_pool],
      is passed to the domain that runs [f] and never waits for it. A worker
      does not inherit the task of the fiber that submitted its work, so the
      message goes above the rows unless the worker made a task current with
      {!Console.Task.within}. Past 4 MiB of lines waiting, the oldest are
      dropped, and a line says how many.

    A message the display cannot take goes to the reporter installed before
    [run], so that a log call never fails. Such a message is one of:
    - a message logged after the display closed, through a reporter kept from
      [f];
    - a message logged outside any fiber of the domain that runs [f], as from a
      thread or a signal handler.

    A message that another domain logs at the very moment [f] ends may be lost.

    That reporter is put back when [f] ends, and when the program exits during
    [f], so what an [at_exit] function logs after the display closed goes to it.

    {2:domains Other domains}

    A worker, such as one of an [Eio.Executor_pool], reports as a fiber of [f]
    does: {!Console.Task} functions, {!print}, {!with_output}, {!redraw} and
    {!run} reach the domain that runs [f], which draws within a tenth of a
    second unless it is computing without yielding.

    [Fmt.pr] and [Fmt.epr] are the exception. In another domain they write on
    that domain's own formatter, which [run] does not take, so their lines land
    among the rows and can leave a stale row on the screen. A worker prints with
    {!print}, {!with_output} or [Logs] instead. Writing on {!Fmt.stdout} or
    {!Fmt.stderr} themselves from another domain is not safe with or without
    console: [Format] may raise [Invalid_argument].

    {2:signals Signals and exits}

    The display closes when the program exits during [f], and signals left at
    their default disposition are handled as follows.

    - SIGINT or SIGTERM during [f]: the display closes once the frame it is
      writing, if any, is written, then the signal ends the program as it would
      have.
    - SIGTSTP (Ctrl-Z): the rows are taken off the terminal and the cursor is
      shown before the program stops, and the rows are drawn again once [fg]
      continues it. In a process group that no shell controls, where the stop
      does not take effect, the rows are drawn again at once and the program
      continues.

    A signal the program handles or ignores is left to it.

    The display belongs to the domain that runs [f]. When it cannot be reached
    from that domain, the terminal is given up rather than restored. That is the
    case for a signal handled in another domain, a write that does not end
    within a second, and an [exit] from another thread or domain or while a
    write is under way. A fixed sequence is then written once, without blocking,
    on the display's terminal, which [run] opens again when it starts. The
    sequence ends an escape code cut in half, a link, the colours and a
    synchronized update, and shows the cursor. The rows stay as they are and the
    display writes nothing more. The signal then ends or stops the program, and
    a program continued after such a stop draws the rows afresh. A write the
    kernel already holds cannot be stopped and may still follow the sequence.

    On {!Fmt.stdout} or {!Fmt.stderr}, a signal handler writes the last frame
    itself. An Eio flow can be written only from a fiber, so on a formatter that
    writes through one, a signal handler that runs outside any fiber leaves the
    last frame to a fiber of the run. When none runs within a second, as while
    the program computes, the frame is lost and the cursor may stay hidden.

    A closed pipe on [ppf] ends the display's output and nothing else, while a
    closed pipe on standard output fails the program's own writes, as without
    console ({!Console.Display}).

    {2:nested Nested runs}

    A [run] called while another runs, in any domain, passes its own function
    the task current where it is called, or the root of the run: so a library
    that calls [run] adds its tasks under the task of its caller. This holds
    inside [f], in a fiber that [f] started and in a fiber beside it, so a
    library can call [run] without knowing whether its caller already did.

    - The [clock], [ppf], [layout], [label], [view], [keep_ended], [commit],
      [plain], [delay] and [progress] of the inner [run] are ignored, since the
      display keeps those of the first [run], and the inner [run] ends no task.
    - The display closes once the last of their functions ends, and the first
      [run] waits for the others.
    - A [run] that starts while that display closes opens its own once it is
      gone. So does one from a fiber that outlived the display it was started
      under, unless another display runs then, which it joins.

    {2:deterministic A deterministic clock}

    [CONSOLE_DETERMINISTIC_CLOCK], set in the environment to a value other than
    the empty string, makes the time the display reads depend on the program's
    sleeps alone, so that the progress a test captures from the program reads
    the same on every machine.

    On a clock that {!clock} made, the time is virtual, as on a mock clock, and
    never the real time. It starts at zero, and a sleep that starts at time [t]
    for [d] wakes at [t + d], so sleeps in different fibers overlap. Sleeps wake
    one at a time, by wake-up time and then in the order they started. While the
    fiber that woke last and the fibers it makes runnable run, the clock reads
    its wake-up time. Each sleep also lasts at least [d] on the clock behind it,
    so the rows move as the program runs, and a sleep whose real duration has
    ended waits for those due before it. What the program does between two
    sleeps, and how long that takes, then does not change the times it shows or
    the [time] and [elapsed] of its {{!Console.Task.json}JSON events}, unless
    the program waits in between, on input or output for example: a sleep can
    then wake meanwhile and move the time from which the next sleep starts.

    The display redraws at instants of the virtual time, every tenth of a second
    while the program sleeps, and its redraws never move that time, so on a
    terminal it draws the same frames on every run.

    [Eio.Time.with_timeout] on such a clock times out once its duration has
    passed on the clock behind it and the sleeps due before it have woken, and a
    timeout that fires moves the time as a sleep does. Under [CONSOLE_CAST], it
    times out as soon as the sleeps due before it have woken.

    On any other clock the time stays at zero:
    - every elapsed time reads [0.0s], and every [time] and [elapsed] of a JSON
      event reads [0];
    - no rate shows;
    - the spinners and the motion stand still;
    - the display redraws on that clock.

    [CONSOLE_CAST], set to a file name, records the run in that file instead of
    on its output, with the clock of [CONSOLE_DETERMINISTIC_CLOCK] whether that
    is set or not. The file is an
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2} recording
    of a terminal of [$COLUMNS] by [$LINES] (80 by 24 without them). It holds
    what the run writes on [ppf], and on both standard outputs when [ppf] is
    one, from then until the program exits: the rows, which [`Auto] draws as on
    a terminal, each write at its time on the clock, and each line feed preceded
    by a carriage return, as the driver of a terminal writes it. The program's
    sleeps then take no real time, so that a recording takes none;
    [CONSOLE_DETERMINISTIC_CLOCK] alone keeps the real pace that a recorder of
    the terminal, such as [k7 rec], films. A file that cannot be opened ends the
    program before the run, with an error message and exit status [1].
    {!Console_tty.setup} starts the recording too, so that what a program prints
    before a run, or without one, is recorded at time zero. *)

val with_output : (Format.formatter -> 'a) -> 'a
(** [with_output f] is {!Console.Display.with_output} on the display of the run
    of the calling fiber, to print what console does not see, such as the text
    of [print_string] or of a child process: [f] is called with the display's
    formatter while the region is off the screen. Outside a run, it is
    [f Fmt.stderr]. *)

val print : Console.View.t -> unit
(** [print v] is {!Console.Display.print} on the display of the run of the
    calling fiber. It prints [v] above the rows, in their scrollback, styled and
    as wide as the terminal; on a pipe, it prints [v] as wide as [$COLUMNS]
    asks, or not cut without it ({!run}), in turn with the lines of the tasks.
    With [`Json] each of its lines, without escape codes, is an [output] event
    that belongs to no task. Outside a run, it is printed on {!Fmt.stderr}, at
    its margin. *)

val redraw : unit -> unit
(** [redraw ()] draws the display of the run of the calling fiber again, at
    once, at the time of its clock, as {!Console.Display.redraw} does: when it
    returns, the display has written the frame of its view for the state of the
    program at that time. A program that must know a frame was drawn, such as
    the last frame of a flash before it ends the run, calls it rather than
    sleeping until the next of the redraws {!val-run} makes ten times a second.
    Outside a run, and where the tasks print as lines and no frame is drawn, it
    does nothing.

    It raises the exception of a view that raised since the last redraw, as
    {!Console.Display.redraw} does. *)

val size : Format.formatter -> (unit -> int * int) option
(** [size ppf] is [Some size] when [ppf] is {!Fmt.stdout} or {!Fmt.stderr} and
    that output is a terminal whose [TERM] is set and not [dumb], as {!run}
    finds out, and [None] otherwise. [size ()] is the terminal's width and
    height, which [$COLUMNS] and [$LINES] override as in {!run}, read again at
    each call, as {!Console.Display.v} takes it:

    {[
    let display ~now ppf view =
      Console.Display.v ?size:(Console_eio.size ppf) ~now ppf view
    ]} *)

val width : Format.formatter -> int option
(** [width ppf] is the width at which {!run} lays out the lines of [ppf]: that
    of the recorded terminal under [CONSOLE_CAST], else [$COLUMNS] when it is
    between [1] and [4096], else the width of the terminal when [ppf] is a
    terminal ({!size}), at most [4096], and [None] otherwise, when a line is not
    cut. Outside a run, a program fits its own lines as a run would by setting
    the margin that {!Console.View.pp} lays a view out in, and prints them at
    their natural size with {!Console.View.to_string} when there is no width:

    {[
    let print v =
      match Console_eio.width Fmt.stdout with
      | None -> Fmt.pr "%s@." (Console.View.to_string v)
      | Some w ->
          Format.pp_set_margin Fmt.stdout w;
          Fmt.pr "%a@." Console.View.pp v
    ]} *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is [c], or, under {{!deterministic}[CONSOLE_DETERMINISTIC_CLOCK]}
    or [CONSOLE_CAST], a virtual clock that starts at zero and moves to the time
    each sleep on it wakes at ({!deterministic}). A sleep until [t] on it lasts
    at least, on [c], the time from the virtual time to [t], so the elapsed
    times, the rates and the spinners of a display that {!run} draws on it
    follow the program's sleeps. *)
