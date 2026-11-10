(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tree of a task, drawn with {!Tree.view}. {!Progress} documents its
    rules. *)

val draws_label : Progress_layout.t -> Task_core.t -> bool
(** [draws_label l r] is [true] iff the row of [r] drawn by [l] has a label, as
    it always has once [r] ended. *)

val measure : layout:Progress_layout.t -> Task_core.t -> unit
(** [measure ~layout r] widens the label column for the rows of [r]'s tree that
    draw a label. *)

val lines : Task_core.t -> int
(** [lines r] is the number of lines the tree of [r] takes when nothing in it
    folds. *)

val view : layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [view ~layout r] is the tree of [r] laid out in its room: [r] and the tasks
    under it, or the top tasks if [r] is the root. In a room with a height, a
    tree too tall first folds the ended tasks under each task into one line,
    then drops lines of the windows, oldest first, then shows only the latest
    running tasks, after a line that counts the others. Without a height, the
    tree is whole, as the scrollback keeps it: every task, none folded, each
    failure or cancellation followed by its output, and a label too wide wrapped
    over the next lines. *)
