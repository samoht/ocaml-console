(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width in terminal cells. *)

val string_width : string -> int
(** [string_width s] is the number of cells that a terminal uses to draw [s], a
    text with no control character. Each grapheme cluster (UAX #29) takes the
    cells of its widest character, by East Asian width (UAX #11):
    - a wide character takes two cells; a combining mark, a format character and
      a control take none; any other character takes one;
    - an emoji sequence takes two cells. A ZWJ sequence, a skin tone, a flag, a
      keycap and a character that VS16 shows as an emoji are emoji sequences;
    - a spacing mark after the first character of its cluster adds the cells
      that it takes alone, as xterm, VTE, Ghostty and iTerm2 draw it. So a
      consonant and its vowel sign take two cells, and a wide mark adds two. A
      spacing mark has general category Mc, or is Thai SARA AM U+0E33 or Lao AM
      U+0EB3, which are letters that UAX #29 treats as spacing marks;
    - a VS16 shows the character before it as an emoji only if that character is
      text by default (a base of emoji-variation-sequences.txt). After any other
      character, a VS16 selects nothing and adds no cell.

    A character with no emoji presentation, such as U+2764 or U+203C, is text
    and takes one cell. A malformed byte takes the cell of U+FFFD.

    Terminals that cluster graphemes, such as kitty and Ghostty, draw text this
    way. Terminals that give each code point its own width, such as xterm and
    VTE, differ. They draw a ZWJ sequence or a skin tone as the sum of its
    emoji, and they draw an emoji that VS16 selects in one cell.
    test/width/widths.txt lists the cells that each terminal uses. *)

val sub_width : string -> pos:int -> len:int -> int
(** [sub_width s ~pos ~len] is [string_width (String.sub s pos len)]. *)

val clusters : string -> string list
(** [clusters s] are the grapheme clusters (UAX #29) of [s], in order. *)

val iter_clusters :
  (offset:int -> len:int -> cells:int -> unit) -> string -> unit
(** [iter_clusters f s] calls [f ~offset ~len ~cells] on each grapheme cluster
    of [s], in order. The cluster is [String.sub s offset len], and [cells] is
    its {!string_width}. *)

val is_spacing_mark : int -> bool
(** [is_spacing_mark c] is [true] iff the code point [c] is a spacing mark
    ({!string_width}). *)

val joins : string -> string -> bool
(** [joins a b] is [true] if a cluster may span the end of [a] and the start of
    [b]. It is [false] if the last cluster of [a] ends where [b] starts; then
    [string_width (a ^ b)] is [string_width a + string_width b]. *)

val is_ascii : string -> bool
(** [is_ascii s] is [true] iff [s] is printable ASCII, one cell per byte. *)
