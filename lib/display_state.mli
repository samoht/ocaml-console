(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The state of a display and of its tasks, which the other display modules
    read and update; {!Display} keeps it abstract. *)

(** The type for how far a task is. *)
type metric = Spin | Count of int * int | Bytes of int64 * int64

type outcome = [ `Succeeded | `Failed | `Cancelled ]
(** The type for task endings. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of Logs messages. *)

(** The type for the kinds of lines. *)
type kind = Output | Logs of level

type line = {
  gap : float;  (** Seconds since the task's previous line. *)
  kind : kind;
  text : string;  (** One line, without control characters. *)
}
(** The type for the lines of a task's output: given to {!Display.log}, a line
    of a standard output among them, or a Logs message logged while it was
    current. *)

(** The type for what a display prints without a terminal: lines in turn, only
    what fails, or one JSON object for each event. *)
type form = Plain | Quiet | Json

val count_text : metric -> string
(** [count_text m] is what a row's count cell shows: items or bytes done over
    the total, or [""] for {!constructor-Spin}. *)

val count_width : metric -> int
(** [count_width m] is the cells [m] needs now and once done, whichever is
    wider, so a count does not walk left as it grows. *)

type t = {
  ppf : Format.formatter;
  size : (unit -> int * int) option;
  form : form;
  depth : Color.depth option;
  now : unit -> float;
  summary : string option;
  layout : Display_layout.t;
  keep : bool;
  start : float;
  mutable width : int;
  mutable height : int;
  running : row Queue.t;
  mutable stale : int;
  mutable count_cell : int;
  by_key : (string, row) Hashtbl.t;
  mutable next_id : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable result : ((string, string) result * string list) option;
  mutable view : (t -> width:int -> height:int -> Span_core.t) option;
  mutable frame : string array;
  mutable frame_widths : int array;
  mutable painted_at : float;
  mutable changes : int;
  mutable painted : int * int * int * int;
  mutable closed : bool;
  mutable closing : bool;
  mutable suspended : int;
  mutable partial : bool;
  writes : string Queue.t;
  mutable writing : bool;
  mutable release : unit -> unit;
  mutable ending : row -> unit;
  mutable root : row option;
  scope : scope;
}

and scope = {
  current : unit -> row option;
  within : row -> (unit -> unit) -> unit;
  cancelled : exn -> bool;
  write_error : exn -> bool;
}

and row = {
  id : int;
  key : string option;
  disp : t;
  parent : row option;
  log_lines : int;
  window : line Queue.t;
  output : line Queue.t;
  mutable output_bytes : int;
  mutable dropped : int;
  mutable log_prev : float;
  mutable label : string;
  mutable metric : metric;
  transient : bool;
  started_at : float;
  mutable ended_at : float option;
  mutable message : string option;
  mutable outcome : outcome option;
  mutable children : row list;
  mutable child_limit : int;
  mutable gone : bool;
  mutable held : int;
  mutable weight : int;
  mutable shed : int;
  mutable last_cur : int64;
  mutable last_t : float;
  mutable rate : float;
}
(** The type for displays and their tasks. {!field-running} holds the tasks in
    the order they started, the running ones and at most as many that ended
    ({!field-stale}); {!field-view} gives the region, the default one if [None],
    and {!field-closing} is [true] while it is drawn for the last time;
    {!field-frame} is the region on the screen, as written; {!field-scope} says
    which task is current. A task's {!field-window} holds its latest lines, at
    most {!field-log_lines}, and on a terminal its {!field-output} holds all its
    lines, {!field-output_bytes} bytes in all, the {!field-dropped} oldest left
    out. {!field-writes} holds, in order, what is to be written on {!field-ppf}
    while {!field-writing} is [true], that is while a write that may yield is
    under way. Without a terminal, {!field-partial} is [true] iff the last byte
    written on {!field-ppf} did not end a line, and {!field-release} gives
    {!field-ppf} its own output functions back, and {!field-form} says what it
    prints. {!field-ending} is called with each task as it ends, before it ends.
    {!field-root} is the top task, the only one with a negative {!field-id}.

    A task's {!field-children} are those started under it, the latest first, a
    top task's those of the root: the running ones and, on a terminal, the ones
    that ended and wait for the scrollback with their top task, or with
    {!field-keep} for the display to close. A task is {!field-gone} once it is
    printed or dropped, and its parent then forgets it. A task that ended and
    waits weighs {!field-weight} bytes, and the ones under a task {!field-held}
    bytes in all; {!field-shed} counts the tasks under it dropped to keep that
    bounded. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics. *)

val room : t -> int
(** [room t] is the width the rows of [t] are laid out in: the terminal's, at
    most 100 columns. *)

val write_error : t -> exn -> bool
(** [write_error t e] is [true] iff [e] is a write error on the output of [t]: a
    [Sys_error], or one that the scope of [t] says is. *)

val is_live : t -> bool
(** [is_live t] is [true] iff [t] draws a region on a terminal. *)

val is_plain : t -> bool
(** [is_plain t] is [true] iff [t] prints its lines in turn, without a terminal.
*)

val keeps_output : t -> bool
(** [keeps_output t] is [true] iff [t] keeps the output of its tasks for their
    failure: on a terminal, and when quiet. *)

val is_running : row -> bool
(** [is_running r] is [true] iff [r] has not ended. *)

val is_root : row -> bool
(** [is_root r] is [true] iff [r] is its display's top task, which never ends.
*)

val elapsed : row -> float
(** [elapsed r] is the seconds [r] ran for, or has run for so far. *)

val depth : row -> int
(** [depth r] is the number of tasks above [r]. *)

val compact : t -> unit
(** [compact t] drops the ended tasks from {!type-t.field-running} once they are
    as many as the running ones. *)

val running_rows : t -> row list
(** [running_rows t] are the running tasks of [t], in the order they started. *)

val running_children : row -> row list
(** [running_children r] are the running tasks directly under [r]. *)

val children : row -> row list
(** [children r] are the tasks under [r] that are not {!field-gone}, in the
    order they started. *)

val root : t -> row
(** [root t] is the top task of [t]. *)
