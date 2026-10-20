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

val text : string -> t
(** See {!Span.text}. *)

val styled : Style.t -> string -> t
(** See {!Span.styled}. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** See {!Span.link}. *)

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

val pp_lines : t list Fmt.t
(** [pp_lines] formats spans one per line, with no line end after the last. *)

val string_of_lines : ?styled:bool -> t list -> string
(** [string_of_lines lines] is [lines] joined by line feeds, each as
    {!to_string} gives it. *)
