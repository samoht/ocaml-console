(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal control for a live region.

    The escape sequences a display draws its region with, and the one region of
    the process that is on the terminal now. *)

(** {1:drawing Drawing} *)

val hide_cursor : string
(** [hide_cursor] is the sequence that hides the cursor. *)

val show_cursor : string
(** [show_cursor] is the sequence that shows the cursor. *)

val with_unlimited_margin : Format.formatter -> (unit -> 'a) -> 'a
(** [with_unlimited_margin ppf f] is [f ()] with the margin of [ppf] set to
    [max_int], so escapes and long lines pass through unwrapped. [ppf] is
    flushed and its margin restored when [f] returns or raises. *)

val pp_clear : Format.formatter -> int -> unit
(** [pp_clear ppf n] erases the current line and the [n - 1] lines above it,
    leaving the cursor at column 0 of the topmost. It writes nothing if [n] is
    not positive. *)

val pp_block : Format.formatter -> string list -> unit
(** [pp_block ppf lines] writes [lines] separated by ["\r\n"], with no line end
    after the last. *)

val pp_update :
  Format.formatter -> old_lines:string list -> new_lines:string list -> unit
(** [pp_update ppf ~old_lines ~new_lines] rewrites, in place, the lines of
    [new_lines] that differ from [old_lines]. The cursor is on the last line of
    the region before and after; both lists have the same length. *)

val show_cursor_on : Format.formatter -> unit
(** [show_cursor_on ppf] writes {!show_cursor} on [ppf] and flushes, ignoring
    [Sys_error]. *)

(** {1:region The active region} *)

type region = {
  ppf : Format.formatter;  (** The formatter the region is drawn on. *)
  clear : unit -> unit;  (** Erases the region. *)
  redraw : unit -> unit;  (** Draws the region again. *)
}
(** The type for the live region on the terminal. *)

val active : unit -> region option
(** [active ()] is the region on the terminal, if any. *)

val set_active : region option -> unit
(** [set_active r] makes [r] the region on the terminal. *)

val claim : unit -> bool
(** [claim ()] is [true] iff it took the process's one activity claim, which was
    free. *)

val release : unit -> unit
(** [release ()] frees the activity claim. *)

val suspend : (unit -> 'a) -> 'a
(** [suspend f] is [f ()] with the {!active} region cleared before and redrawn
    after, so that [f] can write above it. *)

val restore_terminal : unit -> unit
(** [restore_terminal ()] shows the cursor on the {!active} region's formatter,
    if any. *)

val restore_at_exit : unit -> unit
(** [restore_at_exit ()] registers, once per process, an [at_exit] handler that
    resets attributes, ends the line and shows the cursor on the {!active}
    region's formatter. *)
