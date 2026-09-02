(** Eio integration for terminal-aware Console rendering. *)

module Prompt = Prompt
(** Interactive prompts over Eio flows: a yes/no, a line, or a typed word. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?ctx:Console.Display.ctx ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is the Eio driver for {!Console.Display.run}. It constructs
    the terminal context, or takes the one [ctx] hands it, scopes the refresh
    fiber and Logs reporter, and passes the ordinary {!Console.Display.t} to
    [f]. All task operations are in {!Console.Display}; this module adds no
    parallel display API.

    Calls compose: when [run] is called from inside an existing [run] scope in
    the same fiber tree, [f] receives the existing display. Only the outermost
    call owns terminal configuration and teardown. This lets a library expose a
    complete Console operation without forcing its caller to coordinate display
    ownership.

    [ppf] selects the output formatter and defaults to stdout. Unix terminal
    dimensions and tty detection are automatic. Cursor visibility is restored on
    return, exception and SIGINT.

    [ctx] replaces the context [run] builds, so the formatter, the display
    clock, the geometry and the terminal answer are all the caller's. [run] then
    configures and restores no terminal, because a caller that says which
    surface it draws on has taken that ownership, and the surface it named need
    not be a terminal at all. It is how a command grades its own live region --
    a pinned geometry and a clock that does not move make the frames something a
    diff can hold -- without going around the entry point it ships. Passing
    [ppf] as well raises [Invalid_argument]: a context already carries one.
    [clock] is still required, and still paces the refresh.

    [CONSOLE_FROZEN_CLOCK], set in the environment to any non-empty value, stops
    the display clock at zero, so every elapsed time renders as [0.0s]. A test
    that captures this output would otherwise record how fast the machine that
    ran it was, and fail on a slower one. It replaces the clock [run] reads for
    itself and only that one: a [ctx] has already said what time the display
    reads. *)

val is_tty : unit -> bool
(** [is_tty ()] is [true] iff stdout is connected to a terminal. It is the
    terminal question answered once for every CLI that renders through this
    library: a command choosing a table over a stream of plain records asks the
    console it already draws on, rather than asking Unix itself. *)

val dimensions : unit -> int * int
(** [dimensions ()] is the terminal size as [(columns, rows)]: the [TIOCGWINSZ]
    ioctl, else [$COLUMNS] and [$LINES], else 80 by 24. It always answers, and
    no source of an answer reports less than a single cell in either direction.
*)
