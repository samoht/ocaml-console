(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for a {!Console.Display} when its views, hooks or output raise, and
    when an interrupt cuts its writes short. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_display_core}. *)
