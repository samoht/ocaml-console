(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The lines that show tasks, in a look.

    A task is drawn three ways: {{!live}live}, a running task in a tree;
    {{!ended}ended}, a task that ended in a tree, its count and timer against
    the right edge; and {{!appended}appended}, printed in turn without a
    terminal, compact. A line in a room is at most that wide; without one, it is
    whole. *)

open Task_core

(** {1:rows Tasks} *)

val live :
  Look.t -> time:float -> ?width:int -> Progress_layout.t -> t -> View_core.t
(** [live look ~time ~width layout r] is the row of the running task [r] at
    [time], its label cut to the room the other pieces leave, once they have
    given way in the order of their stages. *)

val tail : Look.t -> t -> (int * View_core.t) list
(** [tail look r] are the lines of [r]'s window, oldest first, each after its
    {!field-seq}. *)

val ended : Look.t -> ?width:int -> whole:bool -> t -> View_core.t list
(** [ended look ~width ~whole r] is the line of [r], which ended: its mark and
    label, then its count and timer in the columns every task uses. A label that
    does not fit is cut, or with [whole] goes on over the lines below. *)

val output : Look.t -> width:int -> t -> View_core.t list
(** [output look ~width r] is the output [r] kept, whole: a line that does not
    fit goes on over the lines below. *)

val finished :
  Look.t -> failed:int -> cancelled:int -> int -> Theme.mark -> View_core.t
(** [finished look ~failed ~cancelled n m] is the line, after the mark [m], that
    stands for [n] tasks that ended. *)

val shed : Look.t -> int -> View_core.t
(** [shed look n] says that [n] tasks were dropped. *)

val more : Look.t -> int -> View_core.t
(** [more look n] says that [n] more tasks run. *)

(** {1:lines Lines printed in turn} *)

val started : Look.t -> t -> View_core.t
(** [started look r] is the line printed when [r] starts without a terminal. *)

val appended : Look.t -> t -> View_core.t
(** [appended look r] is the line printed when [r] ends without a terminal. *)

val labelled : Look.t -> ?width:int -> t -> line -> View_core.t list
(** [labelled look ~width r l] is [l] printed above the region or in turn, after
    [r]'s path, folded to [width] if given. *)

val under : Look.t -> ?width:int -> line -> View_core.t list
(** [under look ~width l] is [l] printed under a task's line. *)

val dropped : Look.t -> int -> View_core.t
(** [dropped look n] says that [n] lines of output were dropped. *)

val message : Look.t -> ?width:int -> line -> View_core.t list
(** [message look ~width l] is the line [l] of no task, after its level's mark,
    folded to [width] if given. *)

(** {1:summary The summary and the verdict} *)

val state : t -> Theme.mark
(** [state root] is the mark of the work: running, or the worst of how the root
    and its tasks ended. *)

val summary : Look.t -> ?width:int -> t -> View_core.t option
(** [summary look ~width root] is the summary line of [root], if it has a label.
    In a room, its time and count give way before the label. *)

val verdict : Look.t -> ?width:int -> t -> View_core.t list
(** [verdict look ~width root] is the line of the message the work ended with,
    if any. *)
