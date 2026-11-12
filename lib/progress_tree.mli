(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tree of a task, drawn with {!Tree.view}. {!Progress} documents its
    rules. *)

val draws_label : Progress_layout.t -> Task_core.t -> bool
(** [draws_label l r] is [true] iff the row of [r] drawn by [l] has a label. It
    is always [true] once [r] ended. *)

val measure : layout:Progress_layout.t -> Task_core.t -> unit
(** [measure ~layout r] widens the label column to fit the rows of the tree of
    [r] that draw a label. *)

val lines : Task_core.t -> int
(** [lines r] is the number of lines the tree of [r] takes when nothing in it
    folds. *)

val view : layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [view ~layout r] is the tree of [r] laid out in its room: [r] and the tasks
    under it, or the top tasks if [r] is the root. In a room with a height, a
    tree that is too tall folds in three steps. First, the tasks that ended
    under each task fold into one line. Next, lines of the windows are dropped,
    oldest first. Last, only the latest running tasks show, after a line that
    counts the others. Without a height, the tree is whole, as the scrollback
    keeps it: every task shows, none is folded, each failure or cancellation is
    followed by its output, and a label too wide wraps onto the next lines. *)
