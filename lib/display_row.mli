(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Rows and their log lines as terminal lines.

    A row is drawn three ways: {{!live}live}, in the region, bounded to one
    terminal row; {{!committed}committed}, in a terminal's history, on the
    block's column grid; and {{!appended}appended}, on a surface with no region,
    compact. Every line is styled iff the session's [styling] is set. *)

open Display_state

val span_line : t -> Span.t -> string
(** [span_line t s] is [s] rendered, with ANSI iff [t.styling]. *)

(** {1:rows Rows} *)

val live : t -> row -> string
(** [live t r] is [r]'s line in the live region: its {!Display_line} pieces with
    the label shortened by words to the room left, at most [t.width] columns. *)

val committed : t -> row -> string
(** [committed t r] is [r]'s permanent line on a terminal: its outcome marker
    and label, then its count and timer cells against the right edge. A label
    that does not fit folds onto further lines, joined by line feeds. *)

val appended : t -> row -> string
(** [appended t r] is [r]'s permanent line on an appending surface: its outcome
    marker, label, count and timer, compact. *)

(** {1:logs Log lines} *)

val tail : t -> row -> string list
(** [tail t r] is [r]'s live log tail, at most [r.log_lines] lines of at most
    [t.width] columns. *)

val committed_log : t -> row -> row_log -> string
(** [committed_log t r l] is [l] as a terminal's history draws it, under [r]'s
    guide. *)

val appended_log : t -> row -> row_log -> string
(** [appended_log t r l] is [l] as an appending surface draws it, under [r]'s
    indent. *)

val event : t -> Display_event.level -> string -> string
(** [event t level msg] is the session message [msg], led by [level]'s marker
    unless [level] is [`Info]. *)
