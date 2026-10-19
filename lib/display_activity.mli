(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshots of running work.

    This is the representation behind {!Display.Activity}. *)

type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]
(** The type for task progress. *)

val equal_progress : progress -> progress -> bool
(** [equal_progress a b] is [true] iff [a] and [b] are the same metric. *)

type log
(** The type for log lines in a task's tail. *)

val log_elapsed : log -> float
(** [log_elapsed l] is the time of [l] in seconds since the session started. *)

val log_level : log -> Display_event.level
(** [log_level l] is the severity of [l]. *)

val log_message : log -> string
(** [log_message l] is the sanitized message of [l]. *)

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

val logs : item -> log list
(** [logs i] are the lines of the task's tail, oldest first. *)

type t
(** The type for snapshots. *)

val items : t -> item list
(** [items a] are the running tasks of [a] in display order. *)

val pp : t Fmt.t
(** [pp] formats a snapshot for diagnostics, in an unspecified format. *)

val of_state : Display_state.t -> t
(** [of_state s] is the snapshot of the unfinished rows of [s]. *)
