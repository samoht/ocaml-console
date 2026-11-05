(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Console_driver.Make} on Eio. *)

include Console_driver.T with type clock = Eio.Time.Mono.ty Eio.Resource.t
