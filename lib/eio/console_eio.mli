(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    {!run} shows the tasks of a program ({!Console.Task}) on the terminal. It
    captures the program's output and its {!Logs} messages, and redraws the rows
    ten times a second. However the program ends, it leaves the terminal as it
    found it. *)

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
(** [run ~clock f] is [f root], with the work under [root] shown on [ppf] until
    [f] ends. Unless [f] already ended [root], [root] succeeds when [f] returns,
    fails when [f] raises, and is cancelled when its fiber is cancelled. An
    exception from [f] is raised again after the display closes.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]. A test
      can pass an [Eio_mock.Clock.Mono] clock and move it by hand.
    - [ppf] is where the tasks show, in its theme (defaults to {!Fmt.stderr}).
    - [progress] says how progress shows (defaults to [`Auto]).
    - [label] names the work on the summary line ({!Console.Task.root}).
    - [view root] is the [view] argument of {!Console.Progress.show}. [layout],
      [keep_ended], [commit], [plain] and [delay] are passed to [show] too.

    [run] first calls {!Console_tty.ensure}. If a view raises, the default view
    replaces it and [run] raises that exception. If a redraw raises it, [f] is
    cancelled. A cancelled run waits at most one second for an output that takes
    nothing. *)

val with_output : (Format.formatter -> 'a) -> 'a
(** [with_output f] is {!Console.Display.with_output} on the display of the run.
    Use it to print what console does not see, such as a child process's output.
    [f] gets the display's formatter while the rows are off the screen. Outside
    a run, it is [f Fmt.stderr]. *)

val print : Console.View.t -> unit
(** [print v] is {!Console.Display.print} on the display of the run: it prints
    [v] above the rows, in their scrollback. Outside a run, it prints [v] on
    {!Fmt.stderr}. *)

val redraw : unit -> unit
(** [redraw ()] is {!Console.Display.redraw} on the display of the run: when it
    returns, the frame is written. When a program must know that a frame was
    drawn, it calls [redraw] instead of sleeping. It does nothing outside a run,
    or when the tasks print as lines.

    It raises the exception of any view that raised since the last redraw. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is [c], or, in {{!deterministic}deterministic mode}, a virtual
    clock that starts at zero. *)

val domain_mgr : _ Eio.Domain_manager.t -> Eio.Domain_manager.ty Eio.Resource.t
(** [domain_mgr d] is [d], or, in {{!deterministic}deterministic mode}, a domain
    manager that runs each new domain as a fiber of the domain that asks for it.
    In that mode, code that blocks its domain deadlocks: for example a [Mutex]
    or a [Condition] held across a yield, a spin on an [Atomic], or a [run_raw]
    that blocks. Busy work stops the display from drawing until it sleeps or
    yields. *)

(** {1:output Where output goes}

    A task's output is what the program prints while the task is current in the
    fiber that writes: [Logs] messages, {!Console.Task.log} lines, and
    {!Fmt.stdout} and {!Fmt.stderr} when {!run} captures them. It shows under
    the task while the task runs, and stays if the task fails. Output outside
    any task goes above the rows. On a pipe, each line is printed at once, after
    the path of its task.

    [run] captures [Fmt.stdout] and [Fmt.stderr] when they write where the rows
    do, one whole line at a time. With [`Quiet], the output of a task that
    succeeds is not printed. With [`Json], each line is an [output] event. A
    line longer than 64 KiB is captured in pieces.

    Text that [print_string], [Printf] or a child process writes on the terminal
    bypasses the display and garbles the rows. Print such text with
    {!with_output}.

    A [Logs] message from a source other than {!Logs.default}, at a level other
    than [App], starts with the source's name, as in [[http] connected]. If the
    display cannot take a message, for example after it closed, or from a thread
    or a signal handler, the message goes to the reporter installed before
    [run].

    {1:domains Other domains}

    A worker in another domain, such as one of an [Eio.Executor_pool], reports
    as a fiber of [f] does. {!Console.Task} functions, [Logs], {!print},
    {!with_output}, {!redraw} and {!run} reach the domain that runs [f], and
    never wait for it. That domain draws the result within a tenth of a second,
    unless it computes without yielding. A worker does not inherit a task: make
    one current with {!Console.Task.within}. When more than 4 MiB of lines are
    waiting, the oldest are dropped, and a line says how many.

    [Fmt.pr] and [Fmt.epr] are the exception. In another domain, they write on
    that domain's own formatter, and the text lands among the rows.

    {1:signals Signals and exits}

    The display closes when the program exits during [f]. SIGINT and SIGTERM
    close it, then end the program as usual. SIGTSTP (Ctrl-Z) clears the rows
    and shows the cursor before the program stops. When [fg] continues the
    program, the rows are drawn again. A signal that the program handles or
    ignores is left to the program.

    Sometimes the display cannot be reached: a signal arrives in another domain,
    a write does not end within a second, or [exit] is called from another
    thread or domain, or during a write. Then the terminal is given up. A fixed
    sequence, written once, closes what an interrupted write left open and shows
    the cursor. The rows stay as they are, and the display writes nothing more.

    {1:nested Nested runs}

    A [run] called while another run is active, in any fiber or domain, ignores
    its other arguments. It passes its function the task that is current where
    it is called, or the root. So a library can call [run] without knowing
    whether its caller did. The display closes when the last of these functions
    ends. A [run] that starts while the display is closing waits until it is
    gone, then opens its own.

    {1:deterministic Deterministic mode}

    In deterministic mode, a run shows the same frames on every machine and in
    every run, so that a test can compare them. Setting
    [CONSOLE_DETERMINISTIC_CLOCK] to a non-empty value turns it on, and so does
    setting [CONSOLE_CAST]. In this mode:

    + Time is virtual. It starts at zero and moves only when nothing in the
      program can run. It then jumps to the earliest pending sleep on {!clock}.
      It never waits for real time. The display's redraws wait for virtual time
      but do not move it.
    + Domains run as fibers of the domain that starts them ({!domain_mgr}), so
      the whole program is one domain. A clock made by {!clock} raises
      [Invalid_argument] when used from another domain.
    + Files, processes, the network and the terminal stay real.

    While the program waits on something real, such as a child process, the time
    stands still. On a clock that {!clock} did not make, the time stays at zero.
    Outside this mode, {!clock} and {!domain_mgr} return what they are given.

    When [CONSOLE_CAST] is set to a file name, the run is recorded in that file
    as {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}, on a
    terminal of [$COLUMNS] by [$LINES] (80 by 24 if they are unset). If the file
    cannot be opened, the program ends with status [1]. {!Console_tty.setup}
    starts the recording too. *)
