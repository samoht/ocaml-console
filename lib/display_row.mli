(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The lines a display draws, as spans.

    A task is drawn three ways: {{!live}live}, a running task in a tree of the
    region; {{!ended}ended}, a task that ended in a tree, its count and timer
    against the right edge; and {{!appended}appended}, printed in turn without a
    terminal, compact. In a tree a task's line starts with the guides that
    [lasts] draws: whether it is the last line under its parent, then the same
    for each task above it, innermost first, the outermost drawing nothing. *)

open Display_state

(** {1:rows Tasks} *)

val live : t -> width:int -> lasts:bool list -> row -> Span.t
(** [live t ~width ~lasts r] is the line of the running task [r], its label cut
    to the room the other pieces leave; at most [width] cells. *)

val tail : t -> width:int -> lasts:bool list -> row -> (int * Span.t) list
(** [tail t ~width ~lasts r] are the lines of [r]'s window under its line,
    oldest first, each of at most [width] cells, after its {!field-seq}. *)

val ended :
  t -> width:int -> lasts:bool list -> whole:bool -> row -> Span.t list
(** [ended t ~width ~lasts ~whole r] is the line of [r], which ended: its mark
    and label, then its count and timer in the columns every task uses. A label
    that does not fit is cut, or with [whole] goes on over the lines below. Each
    line is at most [width] cells. *)

val output : t -> width:int -> lasts:bool list -> row -> Span.t list
(** [output t ~width ~lasts r] is the output [r] kept, under its line in a tree,
    whole: a line that does not fit goes on over the lines below. *)

val finished :
  t ->
  width:int ->
  lasts:bool list ->
  failed:int ->
  cancelled:int ->
  int ->
  Theme.mark ->
  Span.t
(** [finished t ~width ~lasts ~failed ~cancelled n m] is the line, after the
    mark [m], that stands for [n] tasks that ended, [failed] of which failed and
    [cancelled] were cancelled. *)

val shed : t -> width:int -> lasts:bool list -> int -> Span.t
(** [shed t ~width ~lasts n] is the line that says [n] tasks were dropped. *)

val more : t -> width:int -> int -> Span.t
(** [more t ~width n] is the line that says [n] more tasks run. *)

val appended : row -> Span.t
(** [appended r] is the line printed when [r] ends without a terminal. *)

(** {1:lines Lines of tasks and Logs messages}

    On a terminal each of these is folded to the width of the rows, the lines
    after the first indented under it; without one it is one line. *)

val started : row -> Span.t
(** [started r] is the line printed when [r] starts without a terminal. *)

val labelled : row -> line -> Span.t list
(** [labelled r l] is [l] printed above the region or without a terminal, under
    [r]'s indent and after [r]'s label. *)

val under : row -> line -> Span.t list
(** [under r l] is [l] printed under [r]'s line, under its indent, without its
    label. *)

val dropped : row -> int -> Span.t
(** [dropped r n] is the line that says [n] of [r]'s output lines were dropped.
*)

val message : t -> level -> string -> Span.t list
(** [message t level s] is the Logs message [s] of no task, after [level]'s
    mark, folded to the terminal's width. *)

(** {1:summary The summary and the result} *)

val summary : ?width:int -> t -> Span.t option
(** [summary ~width t] is the summary line of [t], if it has a name: the mark of
    its state, its name, its time and the tasks that ended out of those started.
    On a terminal it is at most [width] cells (defaults to
    {!Display_state.room}), the name kept longest. *)

val verdict : t -> (string, string) result -> Span.t list
(** [verdict t r] is the line of the result [r], wrapped to the terminal. *)

val detail : t -> string list -> Span.t list
(** [detail t lines] are the muted lines of a result's detail, wrapped to the
    terminal. *)
