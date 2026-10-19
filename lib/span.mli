(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styled text.

    A span is a sequence of runs of text, each carrying a {!Style.t} and
    optionally a hyperlink. It is the unit every widget lays out, measured in
    terminal cells ({!Width}). *)

(** {1:spans Spans} *)

type t
(** The type for spans. *)

val empty : t
(** [empty] is the span with no text. *)

val text : string -> t
(** [text s] is [s], unstyled. *)

val styled : Style.t -> string -> t
(** [styled style s] is [s] drawn in [style]. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** [link ~style ~uri label] is [label] drawn in [style] (defaults to
    {!Style.none}) as a hyperlink to [uri]: an OSC 8 hyperlink when rendered
    with ANSI, [label] alone when rendered plain.

    @raise Invalid_argument
      if [uri] is empty or holds a space or a control character. *)

val ( ++ ) : t -> t -> t
(** [s0 ++ s1] is [s0] followed by [s1]. *)

val concat : ?sep:t -> t list -> t
(** [concat ~sep spans] is [spans] in order, [sep] (defaults to {!empty})
    between each two. *)

val with_style : Style.t -> t -> t
(** [with_style base s] is [s] with [base] composed underneath each of its
    styles: [Style.(base + style)] for each run's [style]. *)

val sanitize : ?keep_newlines:bool -> t -> t
(** [sanitize s] is [s] with its text made {!Console.sanitize}d, its styles and
    hyperlinks kept. Line feeds survive unless [keep_newlines] is [false]. *)

val split_lines : t -> t list
(** [split_lines s] are the lines of [s], split at line feeds, styles kept. The
    list is never empty: {!empty} is one empty line. *)

(** {1:measuring Measuring and fitting} *)

val width : t -> int
(** [width s] is the number of terminal cells [s] takes ({!Width.string_width}).
*)

val truncate : int -> t -> t
(** [truncate w s] is the longest prefix of [s] that fits in [w] terminal cells,
    styles and hyperlinks kept. It is {!empty} if [w] is not positive. *)

val wrap : ?hang:int -> int -> t -> t list
(** [wrap w s] is the single-line span [s] broken at spaces and path separators
    into lines of at most [w] cells, styles and hyperlinks kept on every line. A
    word wider than [w] is continued on the next line where the room ends, so
    every character is printed. Every line after the first starts with [hang]
    spaces (defaults to [0]), clamped to half of [w].

    @raise Invalid_argument if [w] is not positive. *)

val hanging : t -> int
(** [hanging s] is the column the text after the first word of [s] starts at:
    its leading spaces, its first word and the spaces after it. It is [0] when
    [s] is one word. [wrap ~hang:(hanging s)] sets each continuation under that
    text, as a label's value hangs beside it. *)

(** {1:fmt Formatting}

    Rendering follows the style renderer of the formatter,
    {!Fmt.style_renderer}, which [Console_eio.setup] sets from [--color],
    [NO_COLOR] and [TERM] ({!Console.style_renderer}). A gradient
    ({!Style.fg_gradient}) colours each cell by its column, column [0] being the
    span's first cell. *)

val pp : t Fmt.t
(** [pp] formats a span, with ANSI styling iff the formatter's style renderer is
    [`Ansi_tty]. *)

val pp_wrapped : t Fmt.t
(** [pp_wrapped] is like {!pp} but a single-line span wider than the formatter's
    margin is {!wrap}ped to it, each continuation hanging under the text after
    its first word ({!hanging}). *)

val to_string : ?styled:bool -> t -> string
(** [to_string s] is [s] rendered with ANSI styling iff [styled] (defaults to
    [false], the plain text of [s]). *)
