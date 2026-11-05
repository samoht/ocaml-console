(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal given back when SIGINT or SIGTERM ends the program, and while
    SIGTSTP stops it. *)

val protect :
  ?idle:((unit -> unit) -> unit) ->
  ?suspend:((unit -> unit) -> unit) ->
  ?resume:(unit -> unit) ->
  ?terminal:Unix.file_descr ->
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
    ignores is left to it.

    The signal handler runs at any safe point: [restore] and [suspend] are
    called through [idle k], which calls [k] at once (the default) or at the
    next point where they are safe. With [idle], they are called in the calling
    domain only. A signal handled in another domain, or one that finds no idle
    point within a second, gives up instead: {!last_resort}, then the signal
    ends or stops the program, and once a stopped program continues, [resume] is
    called (defaults to doing nothing). A SIGINT or SIGTERM that arrives
    meanwhile is part of the same ending. Nothing that [restore] or [suspend]
    raises keeps the signal from going on. *)

val last_resort : unit -> unit
(** [last_resort ()] makes {!is_dead} [true] and writes, once, on the [terminal]
    of each {!protect} under way, a sequence that needs nothing of what wrote
    there: it ends an escape code cut in half, a link, the colours and a
    synchronized update, and shows the cursor. The terminal is expected
    non-blocking: what it does not take at once is dropped. *)

val is_dead : unit -> bool
(** [is_dead ()] is [true] once {!last_resort} was written, until a stopped
    program continues: an output writes nothing then. *)
