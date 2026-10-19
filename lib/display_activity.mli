(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshots of running work.

    This is the representation behind {!Display.Activity}. *)

type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]
(** The type for task progress. *)

type item
(** The type for running tasks. *)

val id : item -> int
(** [id i] is the numeric identity of [i]'s task. *)

val key : item -> string option
(** [key i] is the caller-given identity of [i]'s task, if any. *)

val parent : item -> int option
(** [parent i] is the numeric identity of the parent task, if any. *)

val label : item -> string
(** [label i] is the current label of the task. *)

val progress : item -> progress
(** [progress i] is the current progress of the task. *)

val elapsed : item -> float
(** [elapsed i] is the time in seconds since the task started. *)

val logs : item -> Display_event.t list
(** [logs i] are the [`Log] events of the task's tail, oldest first. *)

type t
(** The type for snapshots. *)

val items : t -> item list
(** [items a] are the running tasks of [a] in display order. *)

val pp : t Fmt.t
(** [pp] formats a snapshot for diagnostics, in an unspecified format. *)

val of_state : Display_state.t -> t
(** [of_state s] is the snapshot of the unfinished rows of [s]. *)
