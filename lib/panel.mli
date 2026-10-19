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
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?title:Span.t ->
  ?subtitle:Span.t ->
  ?padding:int ->
  ?width:int ->
  Span.t ->
  t
(** [v content] is a panel around [content].

    - The border is [border] if given, else [theme]'s (defaults to
      {!Theme.unicode}, whose border is {!Border.rounded}, as {!Table.v}'s).
    - [title] / [subtitle] label the top / bottom edge.
    - [padding] is the internal horizontal padding (default [1]).
    - [width] fixes the width, cutting content and edge labels to keep the box a
      rectangle; by default it is sized to the content.

    @raise Invalid_argument
      if [padding] is negative or [width] cannot hold the border and padding. *)

(** {1:converting Converting} *)

val to_string : ?styled:bool -> t -> string
(** [to_string panel] is [panel] rendered at its natural width, with ANSI
    styling iff [styled] (defaults to [false]). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a panel, with no line end after its last line. No line is wider
    than the formatter's margin, and a panel of fixed [width] that fits the
    margin is a rectangle [width] cells wide. In a box sized to its content, a
    line wider than the room inside the frame wraps at its words ({!Span.wrap}),
    hanging under the text after its first word; in a box of fixed width it is
    cut ({!Span.truncate}), and so are edge labels. *)
