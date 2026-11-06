(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { now : unit -> float; sleep : float -> unit Lwt.t; system : bool }

let v ~now ~sleep = { now; sleep; system = false }
let monotonic () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9
let system = { now = monotonic; sleep = Lwt_unix.sleep; system = true }

(* Lwt has no virtual clock that the program's sleeps would share, so a
   deterministic run reads zero on the system clock. A clock of [v] is the
   caller's, and reads as it says. *)
let elapsed c =
  if c.system && Cast.deterministic () then fun () -> 0.
  else
    let start = c.now () in
    fun () -> c.now () -. start

let sleep c d = c.sleep d
