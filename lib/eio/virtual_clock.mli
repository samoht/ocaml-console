(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The clock of recording mode. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is as {!Console_eio.clock}. *)

val elapsed : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [elapsed c ()] is the seconds a display reads since [elapsed c] was applied:
    zero in recording mode unless [c] is a clock of {!clock}. *)

val recording_time : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [recording_time c ()] is the time of a clock of {!clock}, without counting
    as a use of it, and zero on another clock. *)
