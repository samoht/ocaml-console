(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runs of [Console_eio] on the standard formatter. *)

val capture_stdout : Console.Progress.t -> (Console.Task.t -> unit) -> string
(** [capture_stdout progress f] is what {!Console_eio.run} of [f] writes on
    {!Fmt.stdout} as its [ppf], under [progress], on a mock backend and clock.
    [run] takes the program's lines from {!Fmt.stdout} in quiet and JSON modes,
    so they are captured too. *)
