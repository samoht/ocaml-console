(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The parts of the quickstart programs that do not depend on their scheduler.
*)

val suites : (string * int) list
(** [suites] are the names of the test suites and their numbers of tests. *)

val pause : float
(** [pause] is the seconds a test takes. *)

val label : string
(** [label] is the label of the run. *)

val main : (unit -> unit) -> unit
(** [main run] reads the command line, sets up the terminal with its [--theme]
    and [--color] options, calls [run ()], prints a table of the results and
    exits. *)
