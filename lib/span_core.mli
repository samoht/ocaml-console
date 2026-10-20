(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styled text, with the operations the widgets share. {!Span} is its public
    face and documents the values both have. *)

type t
(** The type for spans. *)

val empty : t
(** See {!Span.empty}. *)

val is_empty : t -> bool
(** [is_empty s] is [true] iff [s] holds no text, not even a character that
    takes no cell. *)

val text : string -> t
(** See {!Span.text}. *)

val styled : Style.t -> string -> t
(** See {!Span.styled}. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** See {!Span.link}. *)

val gradient : ?colors:Color.t list -> string -> t
(** See {!Span.gradient}. *)

val role : Theme.role -> string -> t
(** See {!Span.role}. *)

val with_role : Theme.role -> t -> t
(** See {!Span.with_role}. *)

val ( ++ ) : t -> t -> t
(** See {!Span.( ++ )}. *)

val concat : ?sep:t -> t list -> t
(** See {!Span.concat}. *)

val with_style : Style.t -> t -> t
(** See {!Span.with_style}. *)

val width : t -> int
(** See {!Span.width}. *)

val ellipsis : string
(** [ellipsis] is U+2026, which ends a line {!truncate} cuts. *)

val truncate : ?ellipsis:string -> int -> t -> t
(** [truncate ~ellipsis w s] is like {!Span.truncate} but ends a cut line with
    [ellipsis] (defaults to U+2026), cut to [w] cells when wider. *)

val wrap : int -> t -> t list
(** See {!Span.wrap}. *)

val pp : t Fmt.t
(** See {!Span.pp}. *)

val to_string : ?styled:bool -> t -> string
(** See {!Span.to_string}. *)

(** {1:widgets For the widgets} *)

val lines : t -> t list
(** [lines s] are the lines of [s], without their line feeds. It is never empty.
*)

val color_at : Color.t list -> float -> Color.t
(** [color_at stops p] is the colour at [p], clamped to 0 to 1, of [stops]
    spread evenly from 0 to 1: the {!Color.blend} of the two stops around [p],
    or the stop at [p] itself. [stops] is not empty. *)

val wrap_hanging : int -> t -> t list
(** [wrap_hanging w s] is like {!wrap} but the lines after the first of each
    line start under the text after its first word, at most [w / 2] cells in. *)

val widest_word : t -> int
(** [widest_word s] is the width of the widest run of [s] without a space. *)

val render : look:Look.t -> ?depth:Color.depth -> t -> string
(** [render ~look ~depth s] is [s] drawn in [look] with its styles at [depth],
    or its text alone without [depth]. *)

val drawn : (Look.t -> t) -> t
(** [drawn draw] is the span [draw look] when it is printed in [look], and
    measured as [draw Look.default]. Every [draw look] must take the same cells
    as [draw Look.default], one cluster for one. A part of it that {!truncate}
    or {!wrap} keep is drawn as the same cells of [draw look]. *)

val resolve : Look.t -> t -> t
(** [resolve look s] is [s] with each {!drawn} part drawn in [look]. *)

val recell :
  (cell:int -> (Style.t -> string -> Style.t * string) option) -> t -> t
(** [recell f line] is the line [line] with each cluster [g] in style [s] that
    starts [cell] cells in, when [f ~cell] is [Some r], drawn as [r s g]: a new
    style and glyph. A glyph that does not take the cells of [g], or holds a
    control character, is [g]; a cluster of a {!drawn} part is kept. *)

val capture : look:Look.t -> margin:int -> (Format.formatter -> unit) -> t list
(** [capture ~look ~margin f] are the lines [f ppf] prints on a formatter [ppf]
    in [look]'s theme and UTF-8, at [margin], without colours. A span printed on
    [ppf] ({!pp}) is taken whole, its styles and its parts drawn when printed
    kept, and text printed in a {!Style.styled} keeps its style, laid under the
    styles of such a span; any other text is unstyled. A line end after the last
    line ends it. *)

val pp_lines : t list Fmt.t
(** [pp_lines] formats spans one per line, with no line end after the last. *)

val string_of_lines : ?styled:bool -> t list -> string
(** [string_of_lines lines] is [lines] joined by line feeds, each as
    {!to_string} gives it. *)
