(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the rows of a {!Console.Display}: their columns, and how their
    pieces give way as the terminal narrows. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_progress_row}. *)
