(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Layout of rendered text blocks.

    A block is a multi-line string, typically the output of {!Panel.to_string},
    {!Table.to_string} or {!Tree.to_string}. {!hcat} places blocks side by side,
    padding each to its own width so the columns stay aligned even when lines
    carry ANSI escapes or wide glyphs; {!vcat} stacks them.

    Animated blocks are joined frame by frame:
    [Anim.map (Layout.hcat ~gutter:2) (Anim.all blocks)] samples every block at
    the same elapsed time ({!Anim.all}). The column widths are recomputed each
    frame, so a block that changes width shifts its neighbours; a block of fixed
    width ({!Panel.v}'s [width]) keeps the columns steady. *)

val hcat : ?gutter:int -> ?align:[ `Top | `Bottom ] -> string list -> string
(** [hcat blocks] joins [blocks] side by side into one multi-line string.

    Each block is split on newlines and every line is padded on the right to
    that block's widest line -- ANSI escapes and wide glyphs counted as the
    terminal renders them (see {!Width.string_width}) -- so each column keeps a
    fixed width regardless of styling. Adjacent columns are separated by
    [gutter] spaces (default [1]).

    When the blocks differ in height the shorter ones are padded with blank
    lines: at the bottom for [`Top] (the default), at the top for [`Bottom]. No
    padding or gutter is emitted past the last non-empty column of a row, so
    rows carry no trailing whitespace. [hcat []] is [""]; [hcat [b]] is [b]
    unchanged. *)

val vcat : ?gutter:int -> string list -> string
(** [vcat blocks] joins [blocks] from top to bottom. [gutter] is the number of
    blank lines inserted between adjacent blocks (default [0]). [vcat []] is
    [""].

    @raise Invalid_argument if [gutter] is negative. *)
