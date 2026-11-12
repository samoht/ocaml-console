(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for what {!Console.Progress.show} draws: the lines of the tasks on a
    pipe, and the view in the region of a terminal. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_progress_show}. *)
