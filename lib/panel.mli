(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bordered panels with optional titles.

    A panel is lines of styled text in a {!Border}, with a title on its top edge
    and a subtitle on its bottom one. *)

(** {1:panels Panels} *)

type t
(** The type for panels. *)

val v :
  ?border:Border.t ->
  ?title:Span.t ->
  ?subtitle:Span.t ->
  ?padding:int ->
  ?width:int ->
  Span.t ->
  t
(** [v content] is a panel around [content].

    - The border is [border] if given, else the border of the theme of the
      formatter it is printed on ({!Theme.get}).
    - [title] / [subtitle] label the top / bottom edge, the title in the theme's
      {!Theme.header}.
    - [padding] is the internal horizontal padding (defaults to [1]).
    - [width] fixes the width, cutting content and edge labels to keep the box a
      rectangle; by default it is sized to the content.

    @raise Invalid_argument
      if [padding] is negative or [width] cannot hold the padding and [border],
      or without [border] a frame one cell wide on each side. *)

(** {1:converting Converting} *)

val to_span : ?width:int -> t -> Span.t
(** [to_span ~width panel] is [panel] as a span, to put in a cell, another
    panel, a tree or beside other text: when it is printed, it is laid out as
    {!pp} lays it out at a margin of [width] (defaults to its natural width), in
    the theme and the [Fmt.utf_8] of the formatter it is printed on. Each of its
    lines takes the cells it takes in {!Theme.default} in UTF-8: where the
    formatter's look lays a line out wider or narrower, as a frame of glyphs two
    cells wide does, the line is cut or padded to those cells. A panel whose
    {!v} [width] is wider than [width] has each of its lines cut to [width], as
    {!pp} cuts it to a narrower margin. *)

val to_string : ?styled:bool -> ?width:int -> t -> string
(** [to_string panel] is [panel] as {!pp} formats it at a margin of [width], or
    at its natural width without [width], with colours iff [styled] (defaults to
    [false]). Its lines are joined by line feeds, with none after the last. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a panel, with no line end after its last line. No line is wider
    than the formatter's margin, and a panel of fixed [width] that fits the
    margin is a rectangle [width] cells wide. In a box sized to its content, a
    line wider than the room inside the frame wraps at its words ({!Span.wrap}),
    hanging under the text after its first word; in a box of fixed width it is
    cut ({!Span.truncate}), and so are edge labels. *)
