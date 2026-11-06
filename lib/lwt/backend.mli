(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Console_driver.Make} on Lwt, through {!Fiber}. *)

include Console_driver.T with type clock = unit
