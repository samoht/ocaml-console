(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal given back when SIGINT or SIGTERM ends the program, and while
    SIGTSTP stops it. *)

val protect :
  ?suspend:((unit -> unit) -> unit) ->
  restore:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** [protect ~restore f] is [f ()]. If SIGINT or SIGTERM arrives while [f] runs
    and the program left that signal at its default disposition, [restore] runs,
    innermost first among nested calls, then the signal ends the program as it
    would have. If SIGTSTP arrives and the program left it at its default
    disposition, the process stops as it would have, inside [suspend stop]
    (defaults to [stop ()]), [stop] returning once the process is continued;
    nested calls suspend innermost first. A signal the program handles or
    ignores is left to it. *)
