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
    return, exception and SIGINT. *)
