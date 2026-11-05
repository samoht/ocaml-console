(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The clock of [CONSOLE_DETERMINISTIC_CLOCK]. *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is as {!Console_eio.clock}. *)

val reading_now : Eio.Time.Mono.ty Eio.Resource.t -> unit -> float
(** [reading_now c ()] is the seconds a display reads since [reading_now c] was
    applied: zero under [CONSOLE_DETERMINISTIC_CLOCK] unless [c] is a clock of
    {!clock}. *)

val reading_sleep : Eio.Time.Mono.ty Eio.Resource.t -> float -> unit
(** [reading_sleep c d] is the sleep between two redraws: a sleep on a clock of
    {!clock} does not move its time. *)

val virtual_time : _ Eio.Time.Mono.t -> unit -> float
(** [virtual_time c ()] is the time of a clock of {!clock}, without counting as
    a use of it, and zero on another clock. *)
