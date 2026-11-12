(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The lines that show tasks, in a look.

    A task is drawn in one of three ways. {{!live}Live}: a running task in a
    tree. {{!ended}Ended}: a task that ended, in a tree, in the columns of the
    live rows. {{!appended}Appended}: a compact line printed in turn, without a
    terminal. A line in a room is at most as wide as the room. Without a room,
    it is whole. *)

open Task_core

(** {1:columns Columns} *)

val bar_max : int
(** [bar_max] is the largest width, in cells, of a bar that grows. *)

val bar_min : int
(** [bar_min] is the smallest width, in cells, of a bar that grows. Below it,
    the pieces of its row give way. *)

val narrowest : int
(** [narrowest] is 12: the fewest cells a label column is cut to before the
    pieces of the later stages give way. The label of a task that ended wraps in
    at least this many cells. *)

type columns = {
  label : int;  (** The width in cells of the label column. *)
  least : int;
      (** The fewest cells the label column is cut to before the pieces of the
          later stages give way. *)
  lead : int;  (** The width in cells of the widest spinner frame or mark. *)
  fixed : bool;
      (** [true] iff the layout fixes the width of the label column. A longer
          label is then cut to that width, except in the scrollback. *)
}
(** The type for the columns a row shares with the rows around it. *)

val lead : Look.t -> int
(** [lead look] is the width in cells of the widest spinner frame or mark of
    [look]. *)

val columns : Look.t -> t -> columns
(** [columns look r] are the columns of a row of [r] drawn alone. Its label
    column is as wide as the widest label column of the work of [r]
    ({!Task_core.Work.label_cell}). It can be cut down to {!narrowest} cells, no
    fewer. *)

(** {1:rows Tasks} *)

val live :
  Look.t ->
  time:float ->
  ?width:int ->
  ?columns:columns ->
  Progress_layout.t ->
  t ->
  View_core.t
(** [live look ~time ~width ~columns layout r] is the row of the running task
    [r] at [time], packed from the left. Its label is padded to the label column
    of [columns], if given. Its bar grows to fill the room, up to {!bar_max}
    cells. Its count and its time are right-aligned in their columns. When room
    is short, the bar shrinks to {!bar_min} cells. Then the pieces give way in
    the order of their stages. From the third stage on, the label column takes
    the room the other pieces leave, down to {!type-columns.field-least} cells.
    Once every piece has given way, it takes whatever room is left. A label
    wider than its column first takes the cells of the later pieces that show
    nothing, and is then cut. *)

val tail : Look.t -> t -> (int * View_core.t) list
(** [tail look r] are the lines of the window of [r], oldest first, each paired
    with its {!field-seq}. Their text is muted, except for a warning or an
    error. *)

val ended :
  Look.t ->
  ?width:int ->
  ?columns:columns ->
  whole:bool ->
  t ->
  View_core.t list
(** [ended look ~width ~columns ~whole r] is the line of [r], a task that ended.
    It is the row that {!Progress_layout.ended} draws: the mark, then the label,
    bar, count, rate and time, in the columns of the live rows, giving way as
    they do. With [whole], the label is never cut. The pieces give way until the
    label fits, and once they all have, the label continues on the lines below.
*)

val output : Look.t -> width:int -> t -> View_core.t list
(** [output look ~width r] is the output that [r] kept, whole. A line that does
    not fit continues on the lines below. *)

val finished :
  Look.t -> failed:int -> cancelled:int -> int -> Theme.mark -> View_core.t
(** [finished look ~failed ~cancelled n m] is the line that stands for [n] tasks
    that ended, after the mark [m]. *)

val shed : Look.t -> int -> View_core.t
(** [shed look n] is the line that says [n] tasks were dropped. *)

val more : Look.t -> int -> View_core.t
(** [more look n] is the line that says [n] more tasks are running. *)

(** {1:lines Lines printed in turn} *)

val started : Look.t -> t -> View_core.t
(** [started look r] is the line printed when [r] starts without a terminal. *)

val appended : Look.t -> t -> ended -> View_core.t
(** [appended look r e] is the line printed when [r] ends as [e] without a
    terminal. *)

val labelled : Look.t -> ?width:int -> t -> line -> View_core.t list
(** [labelled look ~width r l] is [l] after the path of [r], printed above the
    region or in turn, folded to [width] if given. *)

val under : Look.t -> ?width:int -> line -> View_core.t list
(** [under look ~width l] is [l] printed under a task's line. *)

val dropped : Look.t -> int -> View_core.t
(** [dropped look n] is the line that says [n] lines of output were dropped. *)

val message : Look.t -> ?width:int -> line -> View_core.t list
(** [message look ~width l] is [l], a line of the root, after the mark of its
    level, folded to [width] if given. *)

(** {1:summary The summary and the verdict} *)

val state : t -> Theme.mark
(** [state root] is the mark of the work: running, or the worst of how the root
    and its tasks ended. *)

val summary : Look.t -> ?width:int -> t -> View_core.t option
(** [summary look ~width root] is the summary line of [root], if it has a label.
    In a room, the time and the count give way before the label. *)

val verdict : Look.t -> ?width:int -> t -> View_core.t list
(** [verdict look ~width root] is the final message of the work, if any. *)
