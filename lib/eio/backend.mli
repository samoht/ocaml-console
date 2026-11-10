(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Console_driver.Make} on Eio. *)

val clock : Eio.Time.Mono.ty Eio.Resource.t Eio.Fiber.key
(** [clock] is bound to the clock that the display of a run reads. *)

include Console_driver.T
