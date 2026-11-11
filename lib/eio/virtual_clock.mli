(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The clock of deterministic mode. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is the same as {!Console_eio.clock}[ c]. *)

val time : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [time c ()] is the time of [c] in seconds, as a display reads and records
    it. In deterministic mode, it is zero unless {!clock} made [c]. *)

val tick : Eio.Time.Mono.ty Eio.Resource.t -> float -> unit
(** [tick c d] waits between two redraws. On a clock made by {!clock}, it waits
    until the program's sleeps move the time forward by [d], because a redraw
    never moves the time itself. *)
