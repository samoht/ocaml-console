(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width in terminal cells. *)

val string_width : string -> int
(** [string_width s] is the cells a terminal takes to draw [s], a text with no
    control character: grapheme clusters (UAX #29) at their East Asian width
    (UAX #11), so that a wide character takes two cells, a combining mark none,
    and an emoji sequence two: a ZWJ sequence, a skin tone, a flag, a keycap,
    and a character VS16 shows as an emoji. A spacing mark (general category Mc)
    after the first code point of its cluster adds a cell, as xterm, VTE and
    Ghostty draw it, so that a consonant and its vowel sign take two. A VS16
    after a character that emoji-variation-sequences.txt does not list selects
    nothing and adds no cell.

    This is how terminals that cluster graphemes draw text, kitty and Ghostty
    among them. Terminals that give each code point its own width, xterm and VTE
    among them, draw a ZWJ sequence or a skin tone as the sum of its emoji, and
    an emoji that VS16 selects in one cell; the cells each takes are in
    test/input/widths.txt. *)

val sub_width : string -> pos:int -> len:int -> int
(** [sub_width s ~pos ~len] is [string_width (String.sub s pos len)]. *)

val clusters : string -> string list
(** [clusters s] are the grapheme clusters (UAX #29) of [s], in order. *)

val is_ascii : string -> bool
(** [is_ascii s] is [true] iff [s] is printable ASCII, a cell a byte. *)
