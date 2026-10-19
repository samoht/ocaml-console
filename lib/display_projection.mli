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
(** [lines t] is the region [t] draws now: the scene, the header, then each live
    row and its tail, within [t.height - 1] lines. When they do not fit, the
    scene goes first, then log lines, oldest first, then rows between the first
    and the most recent, and a marker counts what was hidden. *)

val paint : t -> unit
(** [paint t] redraws the region of [t] on its terminal, rewriting only the
    lines that changed, and calls the
    {{!Display_state.t.frame_waiters}frame waiters} the frame satisfies. It does
    nothing on an appending, closed or suspended display. *)

val paint_if_stale : t -> unit
(** [paint_if_stale t] is {!paint} if the frame on screen is 100 ms old or
    older, so a producer that reports often paints at the eye's pace. *)

val clear : t -> unit
(** [clear t] erases the region of [t]. *)

val commit : t -> string -> unit
(** [commit t s] writes [s] permanently above the region of [t] and repaints the
    region beneath it. *)

val release_frames : t -> unit
(** [release_frames t] calls every frame waiter of [t]. *)

val install : t -> unit
(** [install t] makes [t] the {{!Display_term.active}active region}: it hides
    the cursor, {{!Display_reporter.install}wraps} the Logs reporter and paints
    the first frame. *)
