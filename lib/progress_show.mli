(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Task views and their display. {!Progress} is the public interface. It
    documents the values both modules share. *)

val row : ?layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [row ~layout task] is {!Progress.row}. *)

val tree : ?layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [tree ~layout task] is {!Progress.tree}. *)

val summary : Task_core.t -> View_core.t
(** [summary root] is {!Progress.summary}. *)

val view : ?layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [view ~layout root] is {!Progress.view}. *)

val show_with :
  ?write_error:(exn -> bool) ->
  ?cancelled:(exn -> bool) ->
  ?atomic:((unit -> unit) -> unit) ->
  ?progress:[ `Auto | `Tty | `Plain | `Quiet | `Json ] ->
  ?layout:Progress_layout.t ->
  ?view:View_core.t ->
  ?keep_ended:bool ->
  ?commit:(Task_core.t -> View_core.t option) ->
  ?plain:(Event.t -> View_core.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task_core.t ->
  Display_core.t
(** [show_with ~write_error ~cancelled ~atomic ppf root] is {!Progress.show},
    with three changes to its display. It ends its output on an exception for
    which [write_error] is [true]. It does not keep an exception for which
    [cancelled] is [true] to report in a later call. It writes each update
    through [atomic]. *)
