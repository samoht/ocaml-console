(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Task views and how a display shows them. {!Progress} is their public face
    and documents the values both have. *)

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
(** [show_with ~write_error ppf root] is {!Progress.show}, whose display takes
    as the end of its output an exception that [write_error] says is a write
    error. *)
