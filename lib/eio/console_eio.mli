(** The Eio driver of Console.

    [Console_eio] owns the terminal for {!Console.Display}: it builds the
    display's context from the terminal behind a formatter, drives its refresh
    from an Eio clock, routes {!Logs} into its history and gives the terminal
    back on exit and on a signal. *)

module Prompt = Prompt
(** Interactive prompts over Eio flows: a line, a yes or no, a typed word or a
    secret. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?fd:Eio_unix.Fd.t ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is the Eio driver for {!Console.Display.run} on the terminal.
    It constructs the terminal context, whose clock is [clock] for both its
    reading and its waits, scopes the refresh fiber and Logs reporter, and
    passes the ordinary {!Console.Display.t} to [f]. Worker log messages are
    routed through a one-message queue to this display's domain and completed
    before their logging continuation resumes. Foreign logging domains must run
    an Eio scheduler. Closing the scope releases queued or awaiting log calls; a
    captured reporter used after closure refuses. All task operations are in
    {!Console.Display}; this module adds no parallel display API.

    Calls compose: when [run] or {!run_on} is called from inside an existing
    scope of either in the same fiber tree, [f] receives the existing display.
    Only the outermost call owns terminal configuration and teardown. This lets
    a library expose a complete Console operation without forcing its caller to
    coordinate display ownership.

    [ppf] selects the output formatter and defaults to stdout. [fd] is the
    descriptor [ppf] writes to: it defaults to standard output's for
    {!Fmt.stdout}, standard error's for {!Fmt.stderr}, and none for any other
    formatter, which then draws on no terminal. [`Auto], the default [mode], is
    {!auto}[ ?fd ppf]. The geometry is that of the terminal behind [fd]: its
    columns, and when there is none [$COLUMNS] if that names more than 10
    columns and 80 otherwise; the height is likewise the terminal's rows,
    [$LINES] above 2, or 24. {!Console.Display.with_output} hands that width to
    a formatter-based widget as its margin. Cursor visibility is restored on
    return, exception and SIGINT.

    The display is styled iff [ppf]'s {!Fmt.style_renderer} is [`Ansi_tty].
    Unless [ppf] already renders ANSI or {!setup} configured it, [run] sets that
    renderer for its span to {!Console.style_renderer} of the environment and of
    whether [fd] is a terminal, so a terminal gets colour and [NO_COLOR] and
    [TERM=dumb] are honoured with no setup. A renderer set to [`None] other than
    by {!setup} cannot be told from no renderer at all: use {!setup} to turn
    colour off.

    @raise Invalid_argument
      if a display opened by [run] or {!run_on} outside the current fiber tree
      is still open. *)

val run_on :
  ctx:Console.Display.ctx ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run_on ~ctx f] is {!run} on the context [ctx], so the formatter, the
    display clock and its waits, the geometry and the terminal answer are all
    the caller's, and the refresh waits on [ctx]'s clock. It configures and
    restores no terminal, because a caller that says which surface it draws on
    has taken that ownership, and the surface it named need not be a terminal at
    all. It is how a command grades its own live region -- a pinned geometry and
    a clock the caller moves make the frames something a diff can hold --
    without going around the entry point it ships.

    @raise Invalid_argument as {!run} does. *)

val setup : ?style_renderer:Fmt.style_renderer -> unit -> unit
(** [setup ~style_renderer ()] sets the style renderer of {!Fmt.stdout} and
    {!Fmt.stderr} to {!Console.style_renderer} of [style_renderer] (the value of
    [Fmt_cli.style_renderer]'s [--color] option), the environment and whether
    that output is a terminal, the rule {!run} follows for the formatter it
    draws on. *)

val auto : ?fd:Eio_unix.Fd.t -> Format.formatter -> [ `Activity | `History ]
(** [auto ?fd ppf] is the display [`Auto] chooses on [ppf] under {!run}:
    [`Activity] iff the descriptor [ppf] writes to ([fd], as {!run} defaults it)
    is a terminal and [TERM] is not [dumb], a terminal with no cursor to move;
    [`History] otherwise. A command that draws something else when no live
    region can be drawn asks here, so it and {!run} give one answer. *)
