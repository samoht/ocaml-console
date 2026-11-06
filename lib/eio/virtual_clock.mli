(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The clock of deterministic mode. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is as {!Console_eio.clock}. *)

val elapsed : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [elapsed c ()] is the seconds a display reads since [elapsed c] was applied:
    zero in deterministic mode unless [c] is a clock of {!clock}. *)

val tick : Eio.Time.Mono.ty Eio.Resource.t -> float -> unit
(** [tick c d] is the wait between two redraws: on a clock of {!clock}, until
    the program's sleeps move its time [d] on, as a redraw never moves it. *)

val recording_time : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [recording_time c ()] is the time of a clock of {!clock}, without counting
    as a use of it, and zero on another clock. *)
