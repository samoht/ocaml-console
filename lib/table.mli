(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers and aligned columns.

    A table is a list of {!type-column}s and rows of cells, drawn in a
    {!Border}. When a table is wider than its room, its columns narrow until it
    fits. When even the narrowest columns and the frame do not fit, each line is
    cut with an ellipsis. No column is ever dropped. *)

(** {1:columns Columns} *)

type overflow = [ `Wrap | `Truncate ]
(** The type for what happens to a cell wider than its column. [`Wrap] continues
    on several lines ({!View.wrap}). [`Truncate] keeps one line and ends it with
    an ellipsis ({!View.truncate}). *)

type column
(** The type for column specifications. *)

val column :
  ?align:View.align ->
  ?width:int ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  View.t ->
  column
(** [column header] is a column with the header [header]. The header is drawn in
    the table's header style, and its own styles apply on top.

    - [align] places the cells in the column (defaults to [`Left]);
    - [width] is the column's width in cells at its natural size (defaults to
      the width of its widest cell), within [min_width] and [max_width]. Like
      any column, it narrows to fit, and a cell wider than it overflows;
    - [min_width] and [max_width] bound its width in cells (default: no bound,
      so it can narrow to one cell). The table goes below [min_width] only when
      nothing else fits;
    - [overflow] is what a cell wider than the column does (defaults to
      [`Wrap]). With [`Wrap], the column does not narrow below its widest word
      while another column can still narrow, so words stay whole. For a cell
      laid out in its room, its {!View.fit} [min_width] is used instead of its
      widest word. So a column of {!Bar.sparkline}s narrows to one cell;
    - [shrinkable] is [false] for a column that narrows last (defaults to
      [true]);
    - [style] is the style of its cells (defaults to {!Style.none}).

    @raise Invalid_argument
      if [width], [min_width] or [max_width] is negative, or if [min_width] is
      greater than [max_width]. *)

(** {1:tables Tables} *)

val view :
  ?border:Border.t ->
  ?gap:int ->
  ?header_style:Style.t ->
  ?rules:bool ->
  column list ->
  View.t list list ->
  View.t
(** [view ~rules columns rows] is the table of [rows] under [columns], laid out
    in its room ({!View.fit}). The table is no wider than the room: its columns
    narrow until it fits. Each cell is laid out at the width of its column.

    - The frame is [border] if given, otherwise the border of the formatter's
      theme ({!Theme.get}).
    - [gap] is the number of spaces between two cells (defaults to [2]). In a
      frame, each cell has [gap / 2] spaces on its left and the rest on its
      right, inside the frame's lines.
    - The headers are in [header_style] (defaults to the theme's
      {!Theme.header}).
    - If [rules] is [true] (defaults to [false]), a rule separates each pair of
      rows. It uses the glyphs and the style of the header's rule. When the
      frame is animated ({!Theme.val-motion}), the ends of the rules move with
      its edges.

    A cell may contain line feeds, which start new lines inside its row. If a
    row has fewer cells than there are columns, its last cell spans the
    remaining columns. In a table without a frame, whose border has no glyph for
    vertical lines ({!Border.none}, {!Border.simple}), a line ends at its last
    non-empty cell.

    - All lines of a framed table that fits have the same width.
    - The cells of a right-aligned column end at the same position.
    - A column is as wide as its widest cell. So in a display where the cells
      change, give the columns a [width] to keep the table still.

    @raise Invalid_argument
      if [gap] is negative, or if a row is empty or has more cells than there
      are columns. *)
