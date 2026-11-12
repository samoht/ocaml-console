(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Eio driver behind {!Console_eio.run}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_display}. *)

val report_domains_suite : string * unit Alcotest.test_case list
(** [report_domains_suite] is the Alcotest test suite for Logs messages logged
    in other domains, or through a reporter kept after [run]. Each message goes
    to the display, or to the reporter installed before [run], and its log call
    completes once. *)
