(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bordered panels with optional titles.

    A panel is a box with a border that can contain styled text. *)

(** {1 Types} *)

type t
(** A panel. *)

(** {1 Construction} *)

val v :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?title:Span.t ->
  ?subtitle:Span.t ->
  ?padding:int ->
  ?width:int ->
  ?char_width:(Uchar.t -> int) ->
  Span.t ->
  t
(** [v content] is a panel around [content].

    - The border is [border] if given, else [theme]'s border, else
      {!Border.rounded}.
    - [title] / [subtitle] label the top / bottom edge.
    - [padding] is the internal horizontal padding (default [1]).
    - [width] fixes the width, truncating content and edge labels to keep the
      box rectangular; by default it is sized to the content.
    - [char_width] measures terminal cells (default
      {!Width.default_char_width}).

    {b Raises.} [Invalid_argument] if [padding] is negative or [width] cannot
    hold the border and padding. *)

val lines :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?title:Span.t ->
  ?subtitle:Span.t ->
  ?padding:int ->
  ?width:int ->
  ?char_width:(Uchar.t -> int) ->
  Span.t list ->
  t
(** [lines] is like {!v} but takes the content already split into lines. *)

(** {1 Rendering} *)

val pp : t Fmt.t
(** [pp] renders the panel, following the formatter's style renderer. *)

val to_string : t -> string
(** [to_string panel] is the plain panel. *)

val to_ansi_string : t -> string
(** [to_ansi_string panel] is the panel with ANSI styling. *)

(** {1 Animation} *)

val anim : t -> string Anim.t
(** [anim panel] is [panel] as an {!Anim.t}. It is still ({!to_ansi_string} on
    every frame) unless [panel]'s theme set {!Theme.animated_border} -- as
    {!Theme.matrix} does -- in which case it is {!rain}. *)

val rain : t -> string Anim.t
(** [rain panel] animates [panel]'s frame as Matrix-style ASCII digital rain: a
    bold green head sweeps clockwise round the border, trailing dimmer green,
    and every cell flickers through an ASCII glyph. The frame is deterministic
    in the elapsed time, so re-painting the same instant is stable. The title
    and subtitle are not drawn in this mode. *)
