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

type overflow = [ `Truncate | `Wrap | `Elide ]
(** The type for the handling of content wider than a column's [max_width].

    - [`Truncate] keeps the leading words that fit on one line and drops the
      rest; a value whose first word does not fit is printed whole, over several
      lines, as with [`Wrap]. No word is ever cut.
    - [`Wrap] wraps to several lines.
    - [`Elide] keeps one line that says what it left out ({!Width.elide}), with
      the row's [see] after a cell it cut. *)

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

val v :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  t
(** [v columns] is an empty table. The border is [border] if given, else
    [theme]'s border, else {!Border.single}; the header row defaults to bold. A
    theme with {!Theme.table_row_separators} enabled draws rules between logical
    data rows. *)

val add_row : ?see:string -> Span.t list -> t -> t
(** [add_row ?see cells table] adds a row of styled cells. [see] follows a cell
    of the row an [`Elide] column cuts ({!Width.elide}).

    A row with fewer cells than [table] has columns ends in a cell that spans
    the columns left, the separators between them included: it is measured
    against none of them, takes the overflow, alignment and style of the column
    it starts in, and in a table without edges runs to the end of the line, the
    rendering width or unbounded. A one-cell row spans every column, as a line
    under a row does. In a table without edges a line ends at its last cell that
    is not empty, unpadded.

    @raise Invalid_argument
      if [cells] is empty or has more cells than [table] has columns. *)

val of_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  Span.t list list ->
  t
(** [of_rows columns rows] is a table with all its data at once, each row as
    {!add_row} adds it; styling is as in {!v}. A cell may contain line feeds;
    they produce physical lines inside that logical row. *)

(** {1:converting Converting} *)

val lines : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> Span.t list
(** [lines table] is each physical row of [table] as a styled span, the lines
    {!to_string} renders, for a live screen to draw. [width], when supplied, is
    a hard maximum for every one. A border's gradient is taken along each line.
*)

val to_string :
  ?styled:bool -> ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string
(** [to_string table] is [table] rendered with ANSI styling iff [styled]
    (defaults to [false]). [width], when supplied, is a hard maximum for every
    physical row. *)

val anim : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string Anim.t
(** [anim table] is [table] as an {!Anim.t}: [to_string ~styled:true table] on
    every frame, unless [table]'s theme sets {!Theme.animated_border}, as
    {!Theme.matrix} does. Then the rendered table is framed by the digital rain
    of {!Panel.anim}. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a table fitted to the formatter's margin, following its style
    renderer. No physical row exceeds the margin; fitting preferences never
    override it. *)
