(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runs of [Console_eio] on the standard formatter. *)

val capture_stdout : Console.Progress.t -> (Console.Task.t -> unit) -> string
(** [capture_stdout progress f] is what {!Console_eio.run} writes on
    {!Fmt.stdout} when it runs [f] under [progress], with {!Fmt.stdout} as its
    [ppf], on a mock backend and clock. In quiet and JSON modes, [run] also
    takes the program's lines from {!Fmt.stdout}, so they are captured too. *)
