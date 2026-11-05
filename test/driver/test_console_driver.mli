(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tests of {!Console_driver} on a backend with no scheduler. *)

val suite : string * unit Alcotest.test_case list
(** The test suite. *)
