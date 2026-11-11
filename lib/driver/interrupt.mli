(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Give the terminal back when SIGINT or SIGTERM ends the program, and while
    SIGTSTP stops it. *)

val protect :
  ?idle:((unit -> unit) -> unit) ->
  ?suspend:((unit -> unit) -> unit) ->
  ?resume:(unit -> unit) ->
  ?terminal:Unix.file_descr ->
  restore:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** [protect ~restore f] is [f ()]. While [f] runs, a signal that the program
    left at its default disposition is handled as follows:
    - SIGINT or SIGTERM: [restore] runs, then the signal ends the program as
      usual. Nested calls restore innermost first.
    - SIGTSTP: the process stops as usual, inside [suspend stop] (defaults to
      [stop ()]). [stop] returns when the process continues. Nested calls
      suspend innermost first.

    A signal that the program handles or ignores, with [Sys.signal] or outside
    OCaml, is left to the program.

    The signal handler runs at any safe point, so [restore] and [suspend] run
    through [idle k]. [idle k] calls [k] at once (the default) or at the next
    point where that is safe. With [idle], they run only in the calling domain.
    The handler gives up if the signal arrives in another domain, or if no idle
    point comes within a second. It then calls {!last_resort}, and the signal
    ends or stops the program. When a stopped program continues, [resume] runs
    (defaults to doing nothing). A SIGINT or SIGTERM that arrives meanwhile is
    part of the same ending. If [restore] or [suspend] raises, the signal still
    ends or stops the program. *)

val last_resort : unit -> unit
(** [last_resort ()] sets {!is_dead} to [true]. It then writes a reset sequence,
    once, on the [terminal] of each running {!protect}. The sequence works
    whatever was written before: it ends a half-written escape code, a link, the
    colours and a synchronized update, and it shows the cursor. The terminal
    should be non-blocking: bytes it does not take at once are dropped. *)

val is_dead : unit -> bool
(** [is_dead ()] is [true] from the moment the terminal is given up, by
    {!last_resort} or by a signal, until a stopped program continues. While it
    is [true], the display writes nothing. *)
