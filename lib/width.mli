(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width calculation in terminal cells.

    UTF-8 is decoded with {!Uutf}; the display width of each code point comes
    from a [char_width] hint, defaulting to {!default_char_width}. ANSI escape
    sequences count as zero. *)

val default_char_width : Uchar.t -> int
(** [default_char_width u] is [0] for a control code and [1] for every other
    code point: exact for ASCII and Latin text. Pass an external Unicode-aware
    hint as [char_width] to size wide text -- CJK, emoji -- to two cells. *)

val string_width : ?char_width:(Uchar.t -> int) -> string -> int
(** [string_width s] is the display width of [s] in terminal columns: UTF-8 is
    decoded, each code point measured by [char_width] (default
    {!default_char_width}), and ANSI escape sequences count as zero. *)

val truncate : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [truncate width s] is [s] cut to fit within [width] terminal columns,
    preserving ANSI escape sequences and adding no ellipsis. *)

val pad_right : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_right width s] is [s] padded with spaces on the right to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val pad_left : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_left width s] is [s] padded with spaces on the left to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val center : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [center width s] is [s] centred within [width] columns with spaces on both
    sides, or [s] unchanged if it is already wider. *)

val wrap : ?indent:int -> int -> string -> string
(** [wrap ?indent width text] is [text] word-wrapped to [width] columns, broken
    at spaces, each line prefixed with [indent] spaces (default [0]). *)
