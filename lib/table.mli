(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers, alignment, and borders.

    Tables display data in aligned columns with optional headers and
    customizable borders. *)

(** {1 Types} *)

type align = [ `Left | `Center | `Right ]
(** Column alignment. *)

type overflow = [ `Truncate | `Wrap ]
(** How to handle content that exceeds max_width.

    - [`Truncate]: Cut off with ellipsis
    - [`Wrap]: Wrap to multiple lines (default) *)

type column
(** A column specification. *)

type t
(** A table. *)

(** {1 Column construction} *)

val column :
  ?align:align ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  string ->
  column
(** [column ?align ?min_width ?max_width ?overflow ?style header] is a column
    specification.

    - [align] is the text alignment (default [`Left]).
    - [min_width] is the minimum column width.
    - [max_width] is the maximum column width.
    - [overflow] is how content beyond [max_width] is handled (default [`Wrap]).
    - [shrinkable] (default [true]) lets terminal-width fitting reduce this
      column; set it [false] to keep a column at full width (e.g. a package-name
      column that must never be truncated) so other columns absorb the fitting.
    - [style] is applied to every cell in the column.
    - [header] is the column header text. *)

val column' :
  ?align:align ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  Span.t ->
  column
(** [column'] is like {!column} but takes a styled span for the header. *)

(** {1 Table construction} *)

val v :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  t
(** [v columns] is an empty table. The border is [border] if given, else
    [theme]'s border, else {!Border.single}; the header row defaults to bold. *)

val add_row : Span.t list -> t -> t
(** [add_row cells table] adds a row of styled cells. The number of cells should
    match the number of columns. *)

val add_row_strings : string list -> t -> t
(** [add_row_strings strings table] adds a row of plain strings. *)

val of_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  Span.t list list ->
  t
(** [of_rows columns rows] is a table with all its data at once; styling is as
    in {!v}. *)

val of_string_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  string list list ->
  t
(** [of_string_rows] is like {!of_rows} but takes plain strings. *)

(** {1 Rendering} *)

val pp : ctx:Display.ctx -> t Fmt.t
(** [pp ~ctx] pretty-prints the table. ANSI styling is emitted only when
    [Display.is_tty ctx] is [true]; piped or captured output (including
    {!Display.dumb}) stays plain. The width from {!Display.dimensions} shrinks
    the columns to fit a terminal. *)

val to_string : ctx:Display.ctx -> t -> string
(** [to_string ~ctx table] renders to a string, fitting the width from
    {!Display.dimensions}. *)

(** {1 Animation} *)

val anim : ctx:Display.ctx -> t -> string Anim.t
(** [anim ~ctx table] is [table] as an {!Anim.t}. It is still ({!to_string} on
    every frame) unless [table]'s theme set {!Theme.animated_border} -- as
    {!Theme.matrix} does -- in which case it is {!rain}. *)

val rain : ctx:Display.ctx -> t -> string Anim.t
(** [rain ~ctx table] frames the rendered table with a Matrix-style ASCII
    digital-rain border (see {!Panel.rain}). Deterministic in the elapsed time.
*)
