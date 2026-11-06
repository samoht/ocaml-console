(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The clock of recording mode: an engine of Lwt whose timers are virtual. *)

val use : bool -> unit
(** [use recording] installs the engine with [Lwt_engine.set] if [recording],
    around the current one, or puts back the engine it wraps otherwise. *)

val elapsed : unit -> unit -> float
(** [elapsed () ()] is the seconds since [elapsed ()] was applied: on the
    virtual clock if its engine is installed then, else on the monotonic clock.
*)
