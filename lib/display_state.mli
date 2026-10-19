(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutable state of a display session and its rows.

    Every other display module reads and updates these records; {!Display} keeps
    them abstract. *)

(** {1:metrics Metrics} *)

(** The type for a row's progress. *)
type metric = Spin | Count of int * int | Bytes of int64 * int64

val count_text : metric -> string
(** [count_text m] is the pair a row's count cell holds: items or bytes done
    over the total, or [""] for {!constructor-Spin}. *)

val count_width : metric -> int
(** [count_width m] is the columns [m] needs: the widest of its pair now and the
    pair it ends on, so a count does not walk left as it gains digits. *)

(** {1:state Sessions and rows} *)

type t = {
  dppf : Format.formatter;  (** The output. *)
  plain : bool;
      (** Append-only lines instead of a live region: a surface with no terminal
          behind it. *)
  styling : bool;
      (** Whether the output takes ANSI, read once when the display opens. A
          terminal whose reader turned colour off is still a live region. *)
  depth : Color.depth;  (** The colours the terminal shows. *)
  now : unit -> float;
  wait : float -> unit;
  dimensions : unit -> int * int;
      (** The terminal width and height, re-read each paint. *)
  header : string option;
  default_style : Display_line.t;
  theme : Theme.t;
  palette : Color.t list;
  dstart : float;  (** When the session started. *)
  mutable width : int;
      (** Bounds every emitted line, so nothing wraps onto a second physical
          row. *)
  mutable height : int;
      (** Caps the live region, so a redraw never scrolls a stale copy into
          unreachable scrollback. *)
  mutable rows : row list;  (** In insertion order. *)
  mutable count_cell : int;
      (** The columns every row lays its count pair in: the widest any row has
          needed. It never narrows. *)
  by_key : (string, row) Hashtbl.t;
  mutable events_rev : Display_event.t list;  (** Newest first. *)
  mutable next_task_id : int;
  mutable pending :
    ([ `Ok of string | `Fail of string ] * string option) option;
      (** The verdict, written when the session closes. *)
  mutable finished : int;
  mutable live_height : int;  (** The lines of the region on screen. *)
  mutable last_lines : string list;  (** The lines on screen. *)
  mutable painted_at : float;  (** When the region was last painted. *)
  mutable torn_down : bool;
  mutable claimed : bool;
  mutable suspended : int;  (** Nesting of [with_output]. *)
}
(** The type for display sessions. *)

and row_log = {
  ltime : float;  (** Seconds since the session started. *)
  lgap : float;  (** Seconds since the row's previous log line. *)
  llevel : Display_event.level;
  lmessage : string;
  lkept : bool;
      (** History takes it when it leaves the tail; a window line is dropped. *)
}
(** The type for log lines in a row's tail. *)

and row = {
  rid : int;
  rkey : string option;
  disp : t;
  parent : row option;
  style : Display_line.t;
  log_lines : int;  (** The height of the log tail; [0] for none. *)
  mutable logs : row_log list;  (** The tail, oldest first. *)
  mutable log_prev_t : float;  (** The time of the previous log line. *)
  mutable rlabel : string;
  mutable metric : metric;
  transient : bool;  (** Drawn while it runs; its ending commits no line. *)
  mutable swept : bool;  (** Settled because the row above it did. *)
  rstart : float;
  mutable rfinish : float option;
  mutable rmessage : string option;
  mutable routcome : Display_event.outcome option;
  mutable last_cur : int64;  (** The byte count of the last rate sample. *)
  mutable last_t : float;  (** The time of the last rate sample. *)
  mutable rate : float;  (** Bytes per second, smoothed. *)
}
(** The type for rows, one per task. *)

val pp : t Fmt.t
(** [pp] formats a session for diagnostics, in an unspecified format. *)

(** {1:styling Styling}

    The values of this section style only when the session's
    {{!type-t.field-styling}styling} is set. *)

val color_code : t -> Color.t -> string
(** [color_code t c] is the SGR sequence that sets the foreground to [c] as a
    terminal of [t]'s colour depth shows it. *)

val code : t -> string -> string
(** [code t seq] is [seq], or [""] without styling. *)

val styled : t -> string -> string -> string
(** [styled t seq s] is [Ansi.styled seq s], or [s] without styling. *)

val dimmed : t -> string -> string
(** [dimmed t s] is [Ansi.dimmed s], or [s] without styling. *)

(** {1:rows Rows} *)

val task_path : row -> string list
(** [task_path r] are the labels of the rows from the root down to [r]. *)

val elapsed : t -> row -> float
(** [elapsed t r] is the seconds [r] ran for, or has run for so far. *)

val indent : row -> string
(** [indent r] is two spaces per ancestor of [r]: the flat indent of an appended
    line, which is written before later siblings are known. *)

val guide_prefix : t -> row -> string
(** [guide_prefix t r] is the tree guide a live row draws before itself, from
    its position among its siblings now. *)

val continuation : t -> row -> string
(** [continuation t r] is the guide column beneath [r] that its log tail sits
    under: a pipe at each level where more siblings follow. *)

val children : row -> row list
(** [children r] are the rows directly under [r], in insertion order. *)

val running_children : row -> row list
(** [running_children r] are the {!children} of [r] that have not finished. *)
