(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal geometry and tty detection.

    Eio has no [isatty], so {!is_tty} wraps [Unix.isatty]; the window size comes
    from [Eio_unix.Pty]. *)

val width : Eio_unix.Fd.t option -> int
(** [width fd] is the width in columns of the terminal behind [fd]. It is
    [$COLUMNS] if that is in \[[1];[4096]\], since POSIX XBD 8.3 lets it
    override the terminal; else the [TIOCGWINSZ] ioctl on [fd] if positive; else
    [80]. With [None], no ioctl is made. *)

val height : Eio_unix.Fd.t option -> int
(** [height fd] is like {!width} for the height in rows, from [$LINES], else
    [24]. *)

val asked : unit -> (unit -> int * int) option
(** [asked ()] is the size at which to lay out an output that is not a terminal:
    [Some size] if [$COLUMNS] is in \[[1];[4096]\], where [size ()] is that
    width and {!height}[ None], and [None] otherwise. *)

val is_tty : Eio_unix.Fd.t -> bool
(** [is_tty fd] is [true] iff [fd] is connected to a terminal. *)

val dimensions : Eio_unix.Fd.t option -> int * int
(** [dimensions fd] is [(width fd, height fd)]. *)
