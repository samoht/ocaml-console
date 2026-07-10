(** Eio integration for terminal-aware Console rendering. *)

module Anim = Anim
(** Animation runners. *)

module Display = Display
(** Live progress display runners. *)

module Spinner = Spinner
(** Spinner helpers. *)

module Terminal = Terminal
(** Unix terminal detection and dimensions. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  (Console.Display.ctx -> 'a) ->
  'a
(** [run ~clock ?ppf f] builds a {!Console.Display.ctx} from the Unix terminal,
    passes it to [f], and restores cursor visibility when stdout is a terminal
    and [f] returns, raises, or the process is interrupted with CTRL-C -- so a
    SIGINT never leaves the terminal with a hidden cursor. *)
