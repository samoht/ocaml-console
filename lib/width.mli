(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width calculation in terminal cells.

    Measurement follows Unicode grapheme clusters (UAX #29) and East Asian width
    (UAX #11), including combining marks, ZWJ emoji, flags and variation
    selectors. ANSI escape sequences count as zero. *)

val string_width : string -> int
(** [string_width s] is the display width of [s] in terminal columns: UTF-8 is
    decoded and grapheme clusters are measured as a terminal renders them. ANSI
    CSI/OSC escape sequences count as zero. *)

val truncate : int -> string -> string
(** [truncate width s] is [s] cut to fit within [width] terminal columns,
    preserving ANSI escape sequences, closing an active SGR style or OSC 8
    hyperlink when cut, and adding no ellipsis. *)

val shorten : int -> string -> string
(** [shorten width s] is [s] when it fits in [width] terminal columns, and
    otherwise the longest run of its leading words that does: the words it drops
    are taken from its end, whole. A word ends at a space outside any bracket,
    so a group such as ["(300s bound)"] is dropped as one word. What is kept
    does not end on a separator (a space, a comma, a semicolon, a colon, a
    middle dot) nor on a word that only leads into the ones dropped (an article,
    a preposition, a conjunction: ["building in"] is ["building"]). No word is
    ever cut, so [s] whose first word is wider than [width] is [""]: a path, an
    id or a digest is kept whole or not at all.

    Escape sequences count as zero columns and a style or hyperlink left open is
    closed. *)

val elide : int -> string -> string
(** [elide width s] is [s] when it fits in [width] terminal columns, and
    otherwise [s] on one line that says what it left out. [s] is read as a list
    of items separated by [", "] or by a middle dot (U+00B7) between spaces,
    outside any bracket: [elide] keeps the leading items that fit, then
    [" and N more"] for the N it leaves out. When the first item alone does not
    fit, its leading words that do ({!shorten}) are followed by ["..."] and the
    count. When no word fits, [elide] is ["..."] when that fits, else [""]. *)

val fold : ?split:bool -> int -> string -> string list
(** [fold width s] is [s] broken at spaces into lines of at most [width]
    terminal columns, the way [fold -s] breaks a line, with the spaces at each
    break dropped. [s] that fits, or a [width] of zero or less, is [[s]].

    A word wider than [width] is never cut: it stands whole on a line of its
    own, which is wider than [width]. With [~split:true] it is instead continued
    on the next line where the room ends, so that every line fits and every
    character is still printed, for a surface that cannot let a line wrap.

    A style or hyperlink open at a break is closed at the end of the line and
    opened again at the start of the next. *)

val atoms : string -> (string * int) list
(** [atoms s] is [s] as the pieces a terminal draws it in, each with its width:
    an escape sequence, of width [0], or one grapheme cluster. *)

val pad_right : int -> string -> string
(** [pad_right width s] is [s] padded with spaces on the right to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val pad_left : int -> string -> string
(** [pad_left width s] is [s] padded with spaces on the left to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val wrap : ?indent:int -> int -> string -> string
(** [wrap ?indent width text] is [text] word-wrapped to [width] terminal
    columns, broken at spaces, each line prefixed with [indent] spaces (default
    [0]). A backquoted run is one word, so a command in backquotes is never
    broken across lines; a word wider than a line takes a line of its own. *)
