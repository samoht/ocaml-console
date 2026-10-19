(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
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

    - The border is [border] if given, else [theme]'s border, else
      {!Border.rounded}.
    - [title] / [subtitle] label the top / bottom edge.
    - [padding] is the internal horizontal padding (default [1]).
    - [width] fixes the width, truncating content and edge labels to keep the
      box rectangular; by default it is sized to the content. Either way {!pp}
      draws it no wider than its formatter's margin.

    @raise Invalid_argument
      if [padding] is negative or [width] cannot hold the border and padding. *)

val content_width : ?margin:int -> t -> int
(** [content_width ~margin panel] is the cells each content line of [panel] is
    drawn in when the whole box may take at most [margin] cells (default
    unbounded): the room of a fixed [width], else the widest line or edge label,
    and never more than [margin] leaves inside the border and padding. A line
    that fills it exactly is drawn in full. *)

(** {1:converting Converting} *)

val to_string : ?styled:bool -> t -> string
(** [to_string panel] is [panel] rendered at its natural width, with ANSI
    styling iff [styled] (defaults to [false]). *)

val anim : t -> string Anim.t
(** [anim panel] is [panel] as an {!Anim.t}: [to_string ~styled:true panel] on
    every frame, unless [panel]'s theme sets {!Theme.animated_border}, as
    {!Theme.matrix} does. Then the border is Matrix-style ASCII digital rain: a
    bold green head sweeps clockwise round it, trailing dimmer green, and every
    cell flickers through an ASCII glyph, deterministically in the elapsed time.
    The title and subtitle are not drawn in this mode. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a panel, following the formatter's style renderer. The
    formatter's margin caps the box. In a box sized to its content, a line wider
    than {!content_width} wraps at its words ({!Span.wrap}), each continuation
    hanging under the text after the line's first word ({!Span.hanging}); in a
    box of fixed [width] it is truncated. Edge labels are truncated. *)
