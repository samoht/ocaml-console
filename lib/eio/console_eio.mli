(** Eio integration for terminal-aware Console rendering. *)

module Prompt = Prompt
(** Interactive prompts over Eio flows: a yes/no, a line, or a typed word. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
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
    the terminal context, scopes the refresh fiber and Logs reporter, and passes
    the ordinary {!Console.Display.t} to [f]. All task operations are in
    {!Console.Display}; this module adds no parallel display API.

    Calls compose: when [run] is called from inside an existing [run] scope in
    the same fiber tree, [f] receives the existing display. Only the outermost
    call owns terminal configuration and teardown. This lets a library expose a
    complete Console operation without forcing its caller to coordinate display
    ownership.

    [ppf] selects the output formatter and defaults to stdout. Unix terminal
    dimensions and tty detection are automatic. Cursor visibility is restored on
    return, exception and SIGINT.

    [CONSOLE_FROZEN_CLOCK], set in the environment to any non-empty value, stops
    the display clock at zero, so every elapsed time renders as [0.0s]. A test
    that captures this output would otherwise record how fast the machine that
    ran it was, and fail on a slower one. *)

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
