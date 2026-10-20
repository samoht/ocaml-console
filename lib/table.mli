(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers and aligned columns.

    A table is a list of {!type-column}s and rows of cells, drawn in a
    {!Border}. A table wider than the room it is given narrows its columns until
    it fits; when even the narrowest columns and the frame do not fit, each line
    is cut with an ellipsis rather than a column left out. *)

(** {1:columns Columns} *)

type align = [ `Left | `Center | `Right ]
(** The type for column alignments. *)

type overflow = [ `Wrap | `Truncate ]
(** The type for what a cell wider than its column does: [`Wrap] goes on over
    several lines ({!Span.wrap}), and [`Truncate] keeps one line ended with an
    ellipsis ({!Span.truncate}). *)

type column
(** The type for column specifications. *)

type t
(** The type for tables. *)

val column :
  ?align:align ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  string ->
  column
(** [column header] is a column headed [header], such that:

    - [align] places the cells in the column (defaults to [`Left]);
    - [min_width] and [max_width] bound its width, in cells; the table goes
      below [min_width] only when nothing else fits;
    - [overflow] is what a cell wider than the column does (defaults to
      [`Wrap]);
    - [shrinkable] (defaults to [true]) at [false] makes the column the last to
      narrow;
    - [style] is the style of its cells (defaults to {!Style.none}).

    @raise Invalid_argument
      if [min_width] or [max_width] is negative, or [min_width] is above
      [max_width]. *)

(** {1:tables Tables} *)

val v :
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  Span.t list list ->
  t
(** [v columns rows] is the table of [rows] under [columns]. Its frame is
    [border] if given, else the border of the theme of the formatter it is
    printed on ({!Theme.get}); the headers are in [header_style] (defaults to
    the theme's {!Theme.header}). A cell may hold line feeds, which start new
    lines inside its row.

    A row with fewer cells than there are columns ends in a cell that spans the
    columns left. In a table without a frame ({!Border.none}) a line ends at its
    last cell that is not empty.

    @raise Invalid_argument
      if a row is empty or has more cells than there are columns. *)

(** {1:converting Converting} *)

val to_string : ?styled:bool -> ?width:int -> t -> string
(** [to_string table] is [table] as {!pp} formats it at a margin of [width], or
    at its natural width without [width], with colours iff [styled] (defaults to
    [false]). Its lines are joined by line feeds, with none after the last. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a table, with no line end after its last line. No line is wider
    than the formatter's margin; the lines of a framed table that fits are all
    as wide as one another; and the cells of a right-aligned column end in the
    same column. *)
