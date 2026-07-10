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

(** {1 Rendering}

    Rendering defers to the target formatter's style renderer — set by
    {!Fmt_tty.setup_std_outputs} (commonly via {!Fmt_cli.style_renderer}, wired
    through [ocaml-observe.setup]). There is no global colour toggle: the source
    of truth is [Fmt.style_renderer ppf], which honours
    [--color=auto|always|never] and [NO_COLOR]. *)

val pp : t Fmt.t
(** [pp] pretty-prints the span with ANSI codes when the target formatter's
    style renderer permits, otherwise plain. *)

val pp_plain : Format.formatter -> t -> unit
(** [pp_plain] renders the span without ANSI codes, no matter what the target
    formatter is configured for. *)

val to_string : t -> string
(** [to_string span] renders to a string with ANSI codes. *)

val to_plain_string : t -> string
(** [to_plain_string span] renders to a string without ANSI codes. *)

(** {1 Measurements} *)

val width : ?char_width:(Uchar.t -> int) -> t -> int
(** [width span] is the display width of [span] in terminal columns, excluding
    ANSI escape sequences. *)

(** {1 Animation} *)

val anim : t -> string Anim.t
(** [anim span] is [span] as a still {!Anim.t}: every frame is {!to_string}, so
    a static span drops into an animated {!Layout} alongside moving widgets. *)
