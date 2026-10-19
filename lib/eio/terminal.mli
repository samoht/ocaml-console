(** Terminal geometry, tty detection and signals.

    Eio has no [isatty], termios or signal handling, so those are the smallest
    wrappers over Unix and [Sys]; the window size is Eio's. *)

val width : Eio_unix.Fd.t option -> int
(** [width fd] returns the width in columns of the terminal behind [fd]: the
    [TIOCGWINSZ] ioctl on [fd], else [$COLUMNS], else [80]. [None] asks no
    ioctl. *)

val height : Eio_unix.Fd.t option -> int
(** [height fd] returns the height in rows of the terminal behind [fd]: the
    [TIOCGWINSZ] ioctl on [fd], else [$LINES], else [24]. [None] asks no ioctl.
*)

val is_tty : Eio_unix.Fd.t -> bool
(** [is_tty fd] is [true] iff [fd] is connected to a terminal. *)

val dimensions : Eio_unix.Fd.t option -> int * int
(** [dimensions fd] is [(width fd, height fd)]. *)

val on_interrupt : (unit -> unit) -> (unit -> 'a) -> 'a
(** [on_interrupt cleanup f] runs [f] with a SIGINT and a SIGTERM handler that
    call [cleanup] and then re-raise the signal under the default handler, so a
    CTRL-C or a [kill] still terminates the process but [cleanup] runs first --
    the hook a cursor-hidden region uses to show the cursor again before
    exiting, since the default handler terminates before any [Fun.protect]
    finaliser would run. When [f] returns or raises, the handler installed here
    gives way to the one it replaced, so nested calls each put back what they
    found; a handler [f] installed in its place is the program's own and is left
    standing. *)
