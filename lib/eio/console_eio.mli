(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] runs a {!Console.Display} on the terminal: it finds out
    whether the output is a terminal and how large it is, redraws the progress
    rows ten times a second, prints {!Logs} messages above them, and gives the
    terminal back as it found it when the function returns, raises, or the
    program is interrupted. *)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, a typed word or a
    password. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is [f display] for a display on [ppf] (defaults to
    {!Fmt.stdout}), timed by [clock]. [header], if given, names the work on a
    line above the rows; [theme] (defaults to {!Console.Theme.unicode}) gives
    the colours and marks, and [layout] (defaults to
    {!Console.Display.Line.default}) the pieces of each row.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr}
    and that output is a terminal whose [TERM] is not [dumb]; otherwise each
    line is printed in turn. The display is in colour as {!setup} decides,
    unless [ppf] was set otherwise. When [f] returns, the tasks still running
    succeed; when it raises, they are cancelled and the exception is raised
    again.

    A [run] called inside [f], or in a fiber [f] started, passes its own
    function the same display, so a library can call [run] without knowing
    whether its caller already did.

    @raise Invalid_argument
      if a display opened elsewhere in the program is still running. *)

val setup : ?style_renderer:Fmt.style_renderer -> unit -> unit
(** [setup ~style_renderer ()] turns colour on for {!Fmt.stdout} and
    {!Fmt.stderr} where they are terminals, as {!Console.style_renderer} of
    [style_renderer] (the value of a [--color] option) and the environment
    decide. A program that prints widgets without a display calls it once,
    first. *)

(**/**)

val run_on :
  ctx:Console.Display.ctx ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run_on ~ctx f] is {!run} on the caller's context [ctx], for tests: it sets
    up and gives back no terminal. *)

(**/**)
