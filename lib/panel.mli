(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bordered panels with optional titles.

    A panel is lines of styled text in a {!Border}, with a title on its top edge
    and a subtitle on its bottom one. *)

(** {1:panels Panels} *)

val view :
  ?border:Border.t ->
  ?title:View.t ->
  ?subtitle:View.t ->
  ?padding:int ->
  ?expand:bool ->
  View.t ->
  View.t
(** [view content] is a panel around [content], laid out in its room
    ({!View.fit}). The panel is no wider than the room. Its content is laid out
    in the space that the frame and the padding leave.

    - The border is [border] if given, otherwise the border of the formatter's
      theme ({!Theme.get}).
    - [title] labels the top edge, in the theme's {!Theme.header} style.
      [subtitle] labels the bottom edge.
    - [padding] is the horizontal padding inside the frame (defaults to [1]).
    - [expand] (defaults to [false]) makes the panel as wide as its room. The
      content and the edge labels are then cut, so the box stays a rectangle.
      Without [expand], the panel fits its content.

    When a line is wider than the space inside the frame:
    - in a panel that fits its content, it wraps at its words ({!View.wrap}),
      and the next lines start under the text after its first word;
    - in a panel that expands, it is cut ({!View.truncate}), and so are the edge
      labels.

    A panel that fits in its room is a rectangle. In a room too narrow for its
    border, each line is cut to the room.

    @raise Invalid_argument
      if [padding] is negative, or if the padding and the border are wider than
      a string can hold ({!Sys.max_string_length}). *)
