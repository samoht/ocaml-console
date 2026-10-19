(** Eio driver for {!Console.Display}. *)

val run :
  ctx:Console.Display.ctx ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~sw ~ctx f] is the only Eio-specific display operation. It scopes
    terminal ownership, drives refreshes by waiting on [ctx]'s own clock
    ({!Console.Display.wait}), and routes {!Logs} messages into permanent
    session history. The callback receives the ordinary {!Console.Display.t};
    all task and semantic operations remain in {!Console.Display}. *)
