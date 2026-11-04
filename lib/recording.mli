(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal that [CONSOLE_CAST] records ({!Cast}).

    This module is internal to the [console] package, as {!Cast} is:
    [Console_tty.setup] and [Console_eio.run] start a recording. *)

val start : time:(unit -> float) -> Format.formatter -> bool
(** [start ~time ppf] is [true] iff [CONSOLE_CAST] asks for a recording
    ({!Cast.file}). [ppf] then writes to the recording instead of its output,
    and so do both {!Fmt.stdout} and {!Fmt.stderr} if [ppf] is one of them. Each
    write is an event at [time ()], in seconds. A recording to the same file
    goes on. Otherwise the previous recording ends, and its formatters write
    where they did before it. A file that cannot be opened ends the program:
    [start] prints why on [stderr] and exits with [1]. *)

val decimal : string -> int option
(** [decimal s] is the integer that [s] writes in decimal digits alone, and
    [None] for anything else that [int_of_string_opt] would also read: a sign,
    an underscore, or a hexadecimal, octal or binary prefix. *)

val size : unit -> int * int
(** [size ()] is the size of the terminal recorded: [$COLUMNS] by [$LINES], each
    when it is a {!decimal} in \[[1];[4096]\], else 80 by 24. *)
