(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The samples that matrix 0.1.0's emulator misreads.

    Built against matrix 0.1.0, the properties that read the test terminal
    discard the samples that meet two of its defects, both fixed upstream. This
    module is temporary: remove it and its calls when console moves to matrix
    0.2.0, so that those properties take every sample again. Built against
    another matrix, its functions do nothing. *)

val discard_stray_vs16 : string -> unit
(** [discard_stray_vs16 s] discards the sample being tested, with
    {!Alcobar.bad_test}, when [s] holds a grapheme cluster with a VS16 (U+FE0F)
    that console does not count two cells wide, as after a character with no
    emoji presentation. matrix 0.1.0 draws such a cluster two cells wide
    (invariant-hq/mosaic#89). *)

val discard_dropped_mark : string -> unit
(** [discard_dropped_mark s] discards the sample being tested when [s] holds a
    character of no width, such as a combining mark or a ZWJ, right after a
    printable ASCII character. matrix 0.1.0 drops it from the screen
    (invariant-hq/mosaic@65da7988, #79). *)
