(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal that [CONSOLE_CAST] records ({!Cast}).

    Like {!Cast}, this module is internal to the [console] package.
    [Console_tty.setup] and [Console_eio.run] start a recording. *)

val start : time:(unit -> float) -> Format.formatter -> bool
(** [start ~time ppf] is [true] iff [CONSOLE_CAST] asks for a recording
    ({!Cast.file}). In that case, [ppf] writes to the recording instead of its
    output. If [ppf] is {!Fmt.stdout} or {!Fmt.stderr}, both of them do. Each
    write is an event at [time ()], in seconds. A running recording to the same
    file continues. Otherwise the previous recording ends, and its formatters
    write to their old outputs again. If the file cannot be opened, [start]
    prints the reason on [stderr] and exits the program with code [1]. *)

val decimal : string -> int option
(** [decimal s] is the integer that [s] writes with decimal digits only. It is
    [None] for any other string, including those that [int_of_string_opt] reads:
    with a sign, an underscore, or a hexadecimal, octal or binary prefix. *)

val size : unit -> int * int
(** [size ()] is the size of the recorded terminal, in columns and lines. The
    columns are [$COLUMNS] and the lines [$LINES], when each is a {!decimal} in
    \[[1];[4096]\]. Otherwise they default to 80 and 24. *)
