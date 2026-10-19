(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal given back when SIGINT or SIGTERM ends the program. *)

val protect : restore:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~restore f] is [f ()]. If SIGINT or SIGTERM arrives while [f] runs
    and the program left that signal at its default disposition, [restore] runs,
    innermost first among nested calls, then the signal ends the program as it
    would have. A signal the program handles or ignores is left to it. *)
