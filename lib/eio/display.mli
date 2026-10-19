(** Eio driver for {!Console.Display}. *)

val run :
  ctx:Console.Display.ctx ->
  ?theme:Console.Theme.t ->
  ?layout:Console.Display.Line.t ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~ctx f] is {!Console.Display.run} [~ctx f] with the rows redrawn on
    [ctx]'s clock and {!Logs} messages printed above them. *)
