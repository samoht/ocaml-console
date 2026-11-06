(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a run reads the time on and waits on between two redraws. *)

type t
(** The type for clocks. *)

val v : now:(unit -> float) -> sleep:(float -> unit Lwt.t) -> t
(** [v ~now ~sleep] is as {!Console_lwt.Clock.v}. *)

val system : t
(** [system] is as {!Console_lwt.Clock.system}. *)

val elapsed : t -> unit -> float
(** [elapsed c ()] is the seconds since [elapsed c] was applied: zero on
    {!system} under [CONSOLE_DETERMINISTIC_CLOCK] or [CONSOLE_CAST]. *)

val sleep : t -> float -> unit Lwt.t
(** [sleep c d] is the promise of a wait of [d] seconds on [c]. *)
