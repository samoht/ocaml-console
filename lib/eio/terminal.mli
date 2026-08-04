(** Terminal geometry and tty detection over Unix. *)

val width : unit -> int
(** [width ()] returns the terminal width in columns: the [TIOCGWINSZ] ioctl,
    else [$COLUMNS], else [tput cols], else [80]. *)

val height : unit -> int
(** [height ()] returns the terminal height in rows: the [TIOCGWINSZ] ioctl,
    else [$LINES], else [tput lines], else [24]. *)

val is_tty : unit -> bool
(** [is_tty ()] is [true] iff stdout is connected to a terminal. *)

val dimensions : unit -> int * int
(** [dimensions ()] is [(width (), height ())]. *)

val hide_cursor : Format.formatter -> unit
(** [hide_cursor ppf] writes the escape sequence that hides the terminal cursor
    to [ppf] and flushes. *)

val show_cursor : Format.formatter -> unit
(** [show_cursor ppf] writes the escape sequence that shows the terminal cursor
    to [ppf] and flushes. *)

val on_interrupt : (unit -> unit) -> (unit -> 'a) -> 'a
(** [on_interrupt cleanup f] runs [f] with a SIGINT handler that calls [cleanup]
    and then re-raises SIGINT under the default handler, so a CTRL-C still
    terminates the process but [cleanup] runs first -- the hook a cursor-hidden
    region uses to {!show_cursor} again before exiting, since the default
    handler terminates before any [Fun.protect] finaliser would run. The
    previously installed handler is restored when [f] returns or raises; nested
    calls save and restore the handler each replaced. *)
