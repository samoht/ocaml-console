(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width calculation in terminal cells.

    Default measurement follows Unicode grapheme clusters (UAX #29) and East
    Asian width (UAX #11), including combining marks, ZWJ emoji, flags and
    variation selectors. ANSI escape sequences count as zero. Supplying
    [char_width] selects code-point measurement for a custom terminal policy. *)

val default_char_width : Uchar.t -> int
(** [default_char_width u] is Uucp's Unicode terminal-width hint: [0] for
    non-printing and combining code points, [1] for narrow code points, and [2]
    for wide CJK and emoji code points. *)

val string_width : ?char_width:(Uchar.t -> int) -> string -> int
(** [string_width s] is the display width of [s] in terminal columns: UTF-8 is
    decoded and grapheme clusters are measured as a terminal renders them. ANSI
    CSI/OSC escape sequences count as zero. An explicit [char_width] replaces
    grapheme measurement with the supplied code-point policy. *)

val truncate : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [truncate width s] is [s] cut to fit within [width] terminal columns,
    preserving ANSI escape sequences, closing an active SGR style or OSC 8
    hyperlink when cut, and adding no ellipsis. *)

val ellipsize :
  ?char_width:(Uchar.t -> int) ->
  ?at:[ `Middle | `End ] ->
  int ->
  string ->
  string
(** [ellipsize width s] is [s] fitted into [width] terminal columns with a
    single ellipsis standing where the cut fell. [at] says where it falls.

    [`Middle], the default, replaces the middle, so both ends of the value
    survive: the leading characters that identify a hash, and the trailing ones
    that name a file. [`End] keeps the leading characters alone and marks the
    right-hand edge, for a value whose tail carries nothing a reader needs.

    [s] already within [width] is returned unchanged, and a [width] of one is
    the ellipsis alone.

    Unlike {!truncate}, which ends a value wherever the room does and leaves it
    reading as a line that wraps, this marks the cut. Escape sequences count as
    zero columns and are never cut; a style or hyperlink left open at either end
    is closed. *)

val pad_right : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_right width s] is [s] padded with spaces on the right to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val pad_left : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_left width s] is [s] padded with spaces on the left to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val center : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [center width s] is [s] centred within [width] columns with spaces on both
    sides, or [s] unchanged if it is already wider. *)

val wrap :
  ?char_width:(Uchar.t -> int) -> ?indent:int -> int -> string -> string
(** [wrap ?char_width ?indent width text] is [text] word-wrapped to [width]
    terminal columns, broken at spaces, each line prefixed with [indent] spaces
    (default [0]). *)
