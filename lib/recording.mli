(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal that [CONSOLE_CAST] records ({!Cast}).

    This module is internal to the [console] package, as {!Cast} is:
    [Console_tty.setup] and [Console_eio.run] start a recording. *)

val start : time:(unit -> float) -> Format.formatter -> bool
(** [start ~time ppf] is [true] iff [CONSOLE_CAST] asks for a recording
    ({!Cast.file}), which [ppf] then writes to, with the standard formatters if
    [ppf] is one, each write an event at [time ()], in seconds. A recording of
    the same file goes on; one of another file, or none, ends the last one,
    whose formatters write where they did. A file that cannot be opened ends the
    program: [start] prints why on [stderr] and exits with [1]. *)

val decimal : string -> int option
(** [decimal s] is the integer that [s] writes in decimal digits alone, and
    [None] for anything else that [int_of_string_opt] would also read: a sign,
    an underscore, or a hexadecimal, octal or binary prefix. *)

val size : unit -> int * int
(** [size ()] is the size of the terminal recorded: [$COLUMNS] by [$LINES], each
    when it is a {!decimal} in \[[1];[4096]\], else 80 by 24. *)
