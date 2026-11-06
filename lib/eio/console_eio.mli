(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    {!run} shows the tasks of a program ({!Console.Task}) on the terminal. It
    takes the program's output and its {!Logs} messages, redraws the rows ten
    times a second, and gives the terminal back as it found it however the
    program ends. *)

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
    [f] ends. Unless [f] ended it, [root] succeeds when [f] returns, fails when
    [f] raises and is cancelled when its fiber is. An exception from [f] is
    raised again once the display has closed.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]; a test
      passes an [Eio_mock.Clock.Mono] clock and moves it by hand.
    - [ppf] is where the tasks show (defaults to {!Fmt.stderr}), in its theme.
    - [progress] says how progress shows (defaults to [`Auto]).
    - [label] names the work on the summary line ({!Console.Task.root}).
    - [view] is applied to the root, and [view root] is the [view] of
      {!Console.Progress.show}. [layout], [keep_ended], [commit], [plain] and
      [delay] are [show]'s too.

    [run] first calls {!Console_tty.ensure}. A view that raises is replaced by
    the default one, and [run] raises its exception, cancelling [f] when a
    redraw raises it. A cancelled run waits a second at most for an output that
    takes nothing. *)

val with_output : (Format.formatter -> 'a) -> 'a
(** [with_output f] is {!Console.Display.with_output} on the display of the run,
    to print what console does not see, such as a child process's output. [f]
    gets the display's formatter while the rows are off the screen. Outside a
    run, it is [f Fmt.stderr]. *)

val print : Console.View.t -> unit
(** [print v] is {!Console.Display.print} on the display of the run: it prints
    [v] above the rows, in their scrollback. Outside a run, it prints [v] on
    {!Fmt.stderr}. *)

val redraw : unit -> unit
(** [redraw ()] is {!Console.Display.redraw} on the display of the run: when it
    returns, the frame is written. A program that must know a frame was drawn
    calls it rather than sleeping. It does nothing outside a run or where the
    tasks print as lines.

    It raises the exception of a view that raised since the last redraw. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is [c], or, in {{!deterministic}deterministic mode}, a virtual
    clock that starts at zero. *)

val domain_mgr : _ Eio.Domain_manager.t -> Eio.Domain_manager.ty Eio.Resource.t
(** [domain_mgr d] is [d], or, in {{!deterministic}deterministic mode}, a domain
    manager that runs each new domain as a fiber of the domain that asks for it.
    Code that blocks its domain then deadlocks, such as a [Mutex] or a
    [Condition] held across a yield, a spin on an [Atomic] or a [run_raw] that
    blocks, and busy work keeps the display from drawing until it sleeps or
    yields. *)

(** {1:output Where output goes}

    A task's output shows under it while it runs, and stays if the task fails.
    Output outside a task goes above the rows; on a pipe, each line is printed
    at once after its task's path. Output is what the program prints while a
    task is current in the fiber that writes it: [Logs] messages,
    {!Console.Task.log} lines, and {!Fmt.stdout} and {!Fmt.stderr} when {!run}
    takes them.

    [run] takes them, a whole line at a time, when they write where the rows do.
    With [`Quiet], a succeeding task's output is not printed; with [`Json], each
    line is an [output] event. A line longer than 64 KiB is taken in pieces.

    Text that [print_string], [Printf] or a child process writes on the terminal
    goes past the display and garbles the rows, so print it with {!with_output}.

    A [Logs] message at another level than [App] from a source other than
    {!Logs.default} comes after the source's name, as in [[http] connected]. A
    message the display cannot take, as after it closed or from a thread or a
    signal handler, goes to the reporter installed before [run].

    {1:domains Other domains}

    A worker, such as one of an [Eio.Executor_pool], reports as a fiber of [f]
    does: {!Console.Task} functions, [Logs], {!print}, {!with_output}, {!redraw}
    and {!run} reach the domain that runs [f], and never wait for it. It draws
    within a tenth of a second unless it computes without yielding. A worker
    does not inherit a task: make one current with {!Console.Task.within}. Past
    4 MiB of lines waiting, the oldest are dropped, and a line says how many.

    [Fmt.pr] and [Fmt.epr] are the exception: in another domain they write on
    that domain's own formatter, which lands among the rows.

    {1:signals Signals and exits}

    The display closes when the program exits during [f]. SIGINT and SIGTERM
    close it, then end the program as they would have. SIGTSTP (Ctrl-Z) takes
    the rows off the terminal and shows the cursor before the program stops, and
    draws them again once [fg] continues it. A signal the program handles or
    ignores is left to it.

    When the display cannot be reached, for a signal handled in another domain,
    a write that does not end within a second, or an [exit] from another thread
    or domain or during a write, the terminal is given up: a fixed sequence
    written once ends what a cut write left open and shows the cursor, the rows
    stay as they are, and the display writes nothing more.

    {1:nested Nested runs}

    A [run] while another runs, in any fiber or domain, passes its function the
    task current where it is called, or the root, and ignores its other
    arguments. So a library can call [run] without knowing whether its caller
    did. The display closes once the last of their functions ends. A [run] that
    starts while the display closes opens its own once it is gone.

    {1:deterministic Deterministic mode}

    Deterministic mode makes a run show the same frames on every machine and
    every run, so that a test can compare them. [CONSOLE_DETERMINISTIC_CLOCK],
    set to a non-empty value, turns it on, and so does [CONSOLE_CAST]. In this
    mode:

    + Time is virtual. It starts at zero and moves only when nothing in the
      program can run; it then jumps to the earliest pending sleep on {!clock}.
      It never waits on real time, and the display's redraws wait for it without
      moving it.
    + Domains run as fibers of the domain that starts them ({!domain_mgr}), so
      the whole program is one domain. A clock of {!clock} used from another
      domain raises [Invalid_argument].
    + Files, processes, the network and the terminal stay real.

    While the program waits on something real, such as a child process, the time
    stands still. On a clock that {!clock} did not make, the time stays at zero.
    Outside this mode, {!clock} and {!domain_mgr} return what they are given.

    [CONSOLE_CAST], set to a file name, records the run there as
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}, on a
    terminal of [$COLUMNS] by [$LINES] (80 by 24 without them). A file that
    cannot be opened ends the program with status [1]. {!Console_tty.setup}
    starts the recording too. *)
