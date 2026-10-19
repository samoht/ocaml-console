(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers, alignment and borders.

    A table is a list of {!type-column}s and rows of styled cells, drawn in a
    {!Border} and fitted to the width it is given. *)

(** {1:columns Columns} *)

type align = [ `Left | `Center | `Right ]
(** The type for column alignments. *)

type overflow = [ `Wrap | `Elide ]
(** The type for the handling of content wider than a column's [max_width].

    - [`Wrap] wraps to several lines.
    - [`Elide] keeps one line that says what it left out. *)

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
(** [column ?align ?min_width ?max_width ?overflow ?shrinkable ?style header] is
    a column specification.

    - [align] is the text alignment (default [`Left]).
    - [min_width] is the preferred minimum column width. Terminal fitting may
      reduce it as a last resort rather than overflow.
    - [max_width] is the maximum column width.
    - [overflow] is how content beyond [max_width] is handled (default [`Wrap]).
    - [shrinkable] (default [true]) sets fitting priority. A [false] column is
      protected until shrinkable columns have yielded, but may still be reduced
      rather than violate the output width.
    - [style] is applied to every cell in the column.
    - [header] is the column header text. *)

(** {1:tables Tables} *)

val of_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  Span.t list list ->
  t
(** [of_rows columns rows] is the table of [rows] under [columns]. The frame is
    [border] if given, else [theme]'s, else {!Border.single}; the headers are in
    [header_style] (defaults to {!Style.bold}). A cell may hold line feeds,
    which start new lines inside its row.

    A row with fewer cells than there are columns ends in a cell that spans the
    columns left. In a table without a frame ({!Border.none}) a line ends at its
    last cell that is not empty.

    @raise Invalid_argument
      if a row is empty or has more cells than there are columns. *)

(** {1:converting Converting} *)

val lines : ?width:int -> t -> Span.t list
(** [lines table] is each physical row of [table] as a styled span, the lines
    {!to_string} renders, for a live screen to draw. [width], when supplied, is
    a hard maximum for every one. A border's gradient is taken along each line.
*)

val to_string : ?styled:bool -> ?width:int -> t -> string
(** [to_string table] is [table] rendered with ANSI styling iff [styled]
    (defaults to [false]). [width], when supplied, is a hard maximum for every
    physical row. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a table fitted to the formatter's margin, following its style
    renderer. No physical row exceeds the margin; fitting preferences never
    override it. *)
