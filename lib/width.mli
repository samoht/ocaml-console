(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width in terminal cells. *)

val string_width : string -> int
(** [string_width s] is the cells a terminal takes to draw [s], a text with no
    control character: grapheme clusters (UAX #29) at their East Asian width
    (UAX #11), so that a wide character takes two cells, a combining mark none,
    and an emoji sequence, a flag or a keycap two. *)

val is_ascii : string -> bool
(** [is_ascii s] is [true] iff [s] is printable ASCII, a cell a byte. *)
