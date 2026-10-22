(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tree of a task, as the region draws it and as the scrollback keeps it.
    {!Display} documents the rules; this module gives the lines. *)

open Display_state

val region : t -> width:int -> height:int -> row -> Span.t list
(** [region t ~width ~height r] are the lines of the tree of [r] in the region,
    at most [height], each of at most [width] cells: [r] and the tasks under it,
    or the top tasks if [r] is the root. When they do not fit, the tasks that
    ended under each task fold into one line, then the lines of the windows give
    way, the oldest of all first, then only the latest running tasks show, after
    a line that counts the others. *)

val scrollback : t -> width:int -> row -> Span.t list
(** [scrollback t ~width r] are the lines of the tree of [r] as the scrollback
    keeps them: every task, none folded, each failure or cancellation with its
    output under it, a line too wide going on over the next ones. *)

val tree : ?width:int -> ?height:int -> row -> Span.t
(** See {!Display.tree}. *)

val summary : t -> Span.t
(** See {!Display.summary}. *)

val default_view : t -> width:int -> height:int -> Span.t
(** See {!Display.default_view}. *)

val default_lines : t -> height:int -> Span.t list
(** [default_lines t ~height] are the lines of [default_view t ~height], none of
    which holds a line feed. *)
