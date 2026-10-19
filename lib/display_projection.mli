(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The activity projection: the live region of running rows.

    The region is the header, the running rows and their log tails, capped to
    the terminal height and repainted in place. Permanent lines are
    {{!commit}committed} above it. *)

open Display_state

val lines : t -> string list
(** [lines t] is the region [t] draws now: the header, then each live row and
    its tail, within [t.height - 1] lines. When they do not fit, log lines go
    first, oldest first, then rows between the first and the most recent, and a
    marker counts what was hidden. *)

val paint : t -> unit
(** [paint t] redraws the region of [t] on its terminal, rewriting only the
    lines that changed. It does nothing on an appending, closed or suspended
    display. *)

val paint_if_stale : t -> unit
(** [paint_if_stale t] is {!paint} if the frame on screen is 100 ms old or
    older, so a producer that reports often paints at the eye's pace. *)

val clear : t -> unit
(** [clear t] erases the region of [t]. *)

val commit : t -> string -> unit
(** [commit t s] writes [s] permanently above the region of [t] and repaints the
    region beneath it. *)

val install : t -> unit
(** [install t] hides the cursor and paints the first frame of [t]. *)
