(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The lines a display draws, as spans.

    A task is drawn three ways: {{!live}live}, in the region, on one row of at
    most the terminal's width; {{!committed}committed}, printed above the region
    once it ends, its count and timer against the right edge; and
    {{!appended}appended}, printed in turn without a terminal, compact. *)

open Display_state

(** {1:rows Tasks} *)

val live : t -> lasts:bool list -> row -> Span.t
(** [live t ~lasts r] is [r]'s row in the region, its tree guide drawn from
    [lasts] ({!Display_state.lasts}), its label cut to the room the other pieces
    leave; at most [t.width] cells. *)

val tail : t -> lasts:bool list -> row -> Span.t list
(** [tail t ~lasts r] are the lines of [r]'s window under its row, oldest first,
    each of at most [t.width] cells. *)

val committed : t -> row -> Span.t list
(** [committed t r] is the line printed when [r] ends on a terminal: its mark
    and label, then its count and timer in the columns every task uses. A label
    that does not fit goes on over the lines below. Each line is at most
    [t.width] cells. *)

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

(** {1:summary The header and the result} *)

val header : t -> Span.t option
(** [header t] is the header of [t], if it has one: the mark of its state, its
    name, its time and the tasks that ended out of those started. On a terminal
    it is at most [t.width] cells, the name kept longest. *)

val verdict : t -> (string, string) result -> Span.t list
(** [verdict t r] is the line of the result [r], wrapped to the terminal. *)

val detail : t -> string list -> Span.t list
(** [detail t lines] are the muted lines of a result's detail, wrapped to the
    terminal. *)
