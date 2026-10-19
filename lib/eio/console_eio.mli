(** Eio integration for terminal-aware Console rendering. *)

module Prompt = Prompt
(** Interactive prompts over Eio flows: a yes/no, a line, or a typed word. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?fd:Unix.file_descr ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
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

    [CONSOLE_FROZEN_CLOCK], set in the environment to any non-empty value, stops
    the display clock's reading at zero, so every elapsed time renders as
    [0.0s]; the refresh still waits on [clock]. A test that captures this output
    would otherwise record how fast the machine that ran it was, and fail on a
    slower one. It replaces the reading [run] builds for itself and only that
    one: a context handed to {!run_on} has already said what time the display
    reads.

    @raise Invalid_argument
      if a display opened by [run] or {!run_on} outside the current fiber tree
      is still open. *)

val run_on :
  ctx:Console.Display.ctx ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
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

    @raise Invalid_argument
      if [ctx] was built without [~wait], or as {!run} does. *)

val setup : ?style_renderer:Fmt.style_renderer -> unit -> unit
(** [setup ~style_renderer ()] configures the process's terminal output once,
    before anything is drawn:
    - the style renderer of {!Fmt.stdout} and {!Fmt.stderr} is
      {!Console.style_renderer} of [style_renderer] (the value of
      [Fmt_cli.style_renderer]'s [--color] option), the environment and whether
      that output is a terminal, so [--color], [NO_COLOR] and [TERM] are
      honoured the same way by every command;
    - {!Console.Color.val-depth} is {!Console.Color.depth_of_env} of the
      environment, so a colour a terminal cannot show is drawn as its nearest
      one. *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] is [true] iff [fd] is connected to a terminal. It is the
    terminal question answered once for every CLI that renders through this
    library: a command choosing a table over a stream of plain records asks
    [is_tty Unix.stdout], and one asking whether a person typed it asks
    [is_tty Unix.stdin], of the console it already draws on rather than of Unix
    itself. *)

val auto : ?fd:Unix.file_descr -> Format.formatter -> [ `Activity | `History ]
(** [auto ?fd ppf] is the display [`Auto] chooses on [ppf] under {!run}:
    [`Activity] iff the descriptor [ppf] writes to ([fd], as {!run} defaults it)
    is a terminal and [TERM] is not [dumb], a terminal with no cursor to move;
    [`History] otherwise. A command that draws something else when no live
    region can be drawn asks here, so it and {!run} give one answer. *)

val frame_drawn : Console.Display.t -> int -> unit Eio.Promise.t
(** [frame_drawn display n] is resolved when the terminal holds frame [n] of
    [display] ({!Console.Display.on_frame}): by the refresh that draws it, so a
    fiber that needs a frame on screen before it goes on awaits it rather than a
    span of time. It is resolved at once when that frame is already drawn and
    when [display] draws no frames, and is resolved when [display] closes. *)

val dimensions : unit -> int * int
(** [dimensions ()] is the size of the terminal on standard output as
    [(columns, rows)]: the [TIOCGWINSZ] ioctl, else [$COLUMNS] and [$LINES],
    else 80 by 24. A width of ten columns or less and a height of two rows or
    less are not taken from either source, so the answer is at least 11 by 3. *)
