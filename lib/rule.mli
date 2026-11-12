(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal lines with an optional label.

    A rule separates the parts of a program's output. It is a line as wide as
    the formatter's margin, in the muted colour of its theme ({!Theme.get}). It
    can carry a label. *)

(** {1:rules Rules} *)

val view :
  ?style:Style.t ->
  ?glyph:string ->
  ?align:View.align ->
  ?label:View.t ->
  unit ->
  View.t
(** [view ()] is a horizontal line as wide as its room ({!View.fit}). At its
    natural size, a line with a label has two glyphs on each side of the label,
    and a line without a label is four glyphs long.
    - [glyph] is the glyph the line repeats (defaults to a light horizontal
      line, U+2500). On a formatter that is not {!Fmt.utf_8}, a non-ASCII
      [glyph] is drawn as [-].
    - [style] is its style (defaults to the muted colour of the formatter's
      theme, {!Theme.get}).
    - [label], if given, sits in the line at [align] (defaults to [`Center]),
      with a space on each side. It is cut with an ellipsis to fit the room.

    @raise Invalid_argument
      if [glyph] is not one terminal cell wide, after each control character is
      replaced by a space. *)
