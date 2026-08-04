(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styled text spans.

    A span is a piece of text with associated styling. Spans can be
    concatenated. *)

type t
(** A styled text span. *)

(** {1 Constructors} *)

val text : string -> t
(** [text str] is a plain, unstyled span of [str]. *)

val styled : Style.t -> string -> t
(** [styled style str] is a span of [str] carrying [style]. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** [link ~uri label] is a terminal hyperlink. ANSI rendering emits an OSC 8
    hyperlink while plain rendering emits only [label], so logs and redirected
    output stay clean. [style] styles the visible label. {b Raises.}
    [Invalid_argument] if [uri] is empty or contains a space or control
    character. *)

val empty : t
(** [empty] is the empty span with no content. *)

val space : t
(** [space] is a single space character span. *)

val newline : t
(** [newline] is a newline character span. *)

(** {1 Concatenation} *)

val concat : t list -> t
(** [concat spans] concatenates a list of spans. *)

val ( ++ ) : t -> t -> t
(** [a ++ b] concatenates two spans. *)

val concat_map : ?sep:t -> ('a -> t) -> 'a list -> t
(** [concat_map ~sep f xs] maps [f] over [xs] and concatenates with [sep]. *)

val sanitize : ?keep_newlines:bool -> t -> t
(** [sanitize span] replaces terminal control characters in visible text with
    spaces while preserving styles, hyperlinks, UTF-8, and line feeds.
    [keep_newlines:false] replaces line feeds too. *)

val split_lines : t -> t list
(** [split_lines span] splits [span] at line feeds without losing styles. The
    result is non-empty; an empty span is one empty line. *)

(** {1 Rendering}

    Rendering defers to the target formatter's style renderer — set by
    [Fmt_tty.setup_std_outputs] (commonly via [Fmt_cli.style_renderer], wired
    through [ocaml-observe.setup]). There is no global colour toggle: the source
    of truth is [Fmt.style_renderer ppf], which honours
    [--color=auto|always|never] and [NO_COLOR]. *)

val pp : t Fmt.t
(** [pp] pretty-prints the span with ANSI codes when the target formatter's
    style renderer permits, otherwise plain. *)

val pp_with_style : Style.t -> t Fmt.t
(** [pp_with_style base] is {!pp} with [base] composed underneath each style in
    the span. *)

val pp_plain : Format.formatter -> t -> unit
(** [pp_plain] renders the span without ANSI codes, no matter what the target
    formatter is configured for. *)

val to_string : t -> string
(** [to_string span] is the plain text of [span]. *)

val to_ansi_string : ?style:Style.t -> t -> string
(** [to_ansi_string span] renders [span] with ANSI codes. [style] is composed
    underneath each style in the span. *)

(** {1 Measurements} *)

val width : ?char_width:(Uchar.t -> int) -> t -> int
(** [width span] is the display width of [span] in terminal columns, excluding
    ANSI escape sequences. *)

val truncate : ?char_width:(Uchar.t -> int) -> int -> t -> t
(** [truncate width span] is the longest styled prefix that fits in [width]
    terminal cells. Styles and hyperlinks are preserved. A non-positive width
    gives {!empty}. *)

val wrap : ?char_width:(Uchar.t -> int) -> int -> t -> t list
(** [wrap width span] word-wraps a single-line span at spaces and path
    separators, preserving styles and hyperlinks on every line. An overlong
    token is truncated with an ellipsis. {b Raises.} [Invalid_argument] if
    [width] is not positive. *)

(** {1 Animation} *)

val anim : t -> string Anim.t
(** [anim span] is [span] as a still {!Anim.t}: every frame is
    {!to_ansi_string}, so a static span drops into an animated {!Layout}
    alongside moving widgets. *)
