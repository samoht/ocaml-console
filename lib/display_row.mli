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
(** [tail t ~lasts r] are [r]'s log lines under its row, oldest first, each of
    at most [t.width] cells. *)

val committed : t -> row -> Span.t list
(** [committed t r] is the line printed when [r] ends on a terminal: its mark
    and label, then its count and timer in the columns every task uses. A label
    that does not fit goes on over the lines below. Each line is at most
    [t.width] cells. *)

val appended : row -> Span.t
(** [appended r] is the line printed when [r] ends without a terminal. *)

(** {1:logs Log lines and messages} *)

val committed_log : row -> log -> Span.t
(** [committed_log r l] is [l] printed above the region, under [r]'s indent, at
    most the terminal's width. *)

val appended_log : row -> log -> Span.t
(** [appended_log r l] is [l] printed without a terminal, after [r]'s label. *)

val message : t -> level -> string -> Span.t
(** [message t level s] is [s] after [level]'s mark, unless [level] is [`Info].
*)

(** {1:summary The header and the result} *)

val header : t -> Span.t option
(** [header t] is the header of [t], if it has one: the mark of its state, its
    name, its time and the tasks that ended out of those started. On a terminal
    it is at most [t.width] cells, the name kept longest. *)

val verdict : t -> [ `Ok of string | `Fail of string ] -> Span.t list
(** [verdict t r] is the line of the result [r], wrapped to the terminal. *)

val detail : t -> string list -> Span.t list
(** [detail t lines] are the muted lines of a result's detail, wrapped to the
    terminal. *)
