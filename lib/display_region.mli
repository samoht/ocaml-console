(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The region on the terminal: which lines it shows, and the bytes that draw,
    update and erase it. Each operation writes once. *)

open Display_state

val hide_cursor : string
(** [hide_cursor] is the escape code that hides the cursor. *)

val show_cursor : string
(** [show_cursor] is the escape code that shows the cursor. *)

val fresh_row : t -> string
(** [fresh_row t] is what takes the cursor, at any column, to the first column
    of a blank row: the row it is on if that is the first column, the next one
    otherwise. *)

val synchronized : t -> string -> string
(** [synchronized t s] is [s] as one synchronized update (mode 2026) on a
    terminal, and [s] otherwise. *)

val write : t -> string -> unit
(** [write t s] writes [s] on [t]'s output and flushes it, {!synchronized}, a
    write error ending the output rather than the program. Without a terminal,
    {!Fmt.stdout} is flushed first. While another write is under way, in a fiber
    its output yielded from, [s] is written after it, by that fiber;
    [write t ""] writes what an earlier write cut short left. *)

val eol : t -> string
(** [eol t] is the line end [t] writes: ["\r\n"] on a terminal, ["\n"]
    otherwise. *)

val render : t -> Span.t -> string
(** [render t s] is [s] as [t] writes it: styled at its depth, its frames moving
    by the time since [t] started, or plain and still. *)

val frame : t -> Span.t list
(** [frame t] is the region of [t]: the scene, then the header, then each
    running task and its log tail, at most [t.height - 1] lines. When they do
    not fit, the scene gives way first, from its last line, then log lines, the
    oldest first, then the tasks between the first and the most recent, and a
    marker counts what is hidden. *)

val clear : Buffer.t -> t -> unit
(** [clear buf t] adds to [buf] what erases the region on the screen, counting
    the rows a terminal that reflows its lines gives it at [t.width], and
    records the screen as empty. *)

val refresh_size : t -> bool
(** [refresh_size t] reads the terminal's size again and is [true] iff it
    changed. *)

val paint : t -> unit
(** [paint t] draws the region again, rewriting only the lines that changed, and
    does nothing when no task, time or size it shows changed. It does nothing on
    a display without a terminal, closed, or inside [with_output]. *)

val paint_if_stale : t -> unit
(** [paint_if_stale t] is {!paint} if the region was drawn a tenth of a second
    ago or more. *)

val commit : t -> Span.t list -> unit
(** [commit t lines] prints [lines] once: above the region, which is drawn again
    below them, or in turn without a terminal. *)

val erase : t -> unit
(** [erase t] takes the region off the screen. *)
