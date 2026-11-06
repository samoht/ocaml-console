(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal behind a formatter, and its size. *)

val window_size : Unix.file_descr -> (int * int) option
(** [window_size fd] is the columns and rows of the terminal behind [fd], or
    [None] if [fd] is not a terminal. *)

val is_terminal : Unix.file_descr -> bool
(** [is_terminal fd] is {!Console_tty.is_terminal}. *)

val width : Unix.file_descr option -> int
(** [width fd] is the width in columns of the terminal behind [fd]. It is
    [$COLUMNS] if that is in \[[1];[4096]\], since POSIX XBD 8.3 lets it
    override the terminal; else the [TIOCGWINSZ] ioctl on [fd] if positive; else
    [80]. With [None], no ioctl is made. *)

val height : Unix.file_descr option -> int
(** [height fd] is like {!width} for the height in rows, from [$LINES], else
    [24]. *)

val asked : unit -> (unit -> int * int) option
(** [asked ()] is the size at which to lay out an output that is not a terminal:
    [Some size] if [$COLUMNS] is in \[[1];[4096]\], where [size ()] is that
    width and {!height}[ None], and [None] otherwise. *)

val dimensions : Unix.file_descr option -> int * int
(** [dimensions fd] is [(width fd, height fd)]. *)

val descriptor : Format.formatter -> Unix.file_descr option
(** [descriptor ppf] is what [ppf] writes to, when it is {!Fmt.stdout} or
    {!Fmt.stderr}. *)

val of_ppf : Format.formatter -> Unix.file_descr option
(** [of_ppf ppf] is {!descriptor}[ ppf] if that is a terminal. *)

val routed : Unix.file_descr -> Format.formatter list
(** [routed fd] is the standard formatters on the same terminal as [fd]. *)

val sharing : Unix.file_descr -> Format.formatter list
(** [sharing fd] is the standard formatters that write where [fd] does. *)

val device_names : dev:string list -> pts:string list -> string list
(** [device_names ~dev ~pts] is the paths where {!reopened} looks for a
    terminal, given the names in [/dev] and in [/dev/pts]: [/dev/tty], each of
    [/dev/pts], then the [/dev/tty*], macOS's [/dev/ttys*] first. *)

val reopened : Unix.file_descr -> Unix.file_descr option
(** [reopened fd] is the terminal of [fd] opened again, non-blocking, so that
    the last resort does not change the flags of a descriptor shared with a
    shell. *)

val cursor : Unix.file_descr -> bool -> unit
(** [cursor fd visible] shows or hides the cursor. *)

val size : Format.formatter -> (unit -> int * int) option
(** [size ppf] reads the size of the terminal of {!of_ppf}[ ppf]. *)

val width_of : Format.formatter -> int option
(** [width_of ppf] is the width to fit output on [ppf] to. *)
