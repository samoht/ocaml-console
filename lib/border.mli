(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Box-drawing borders for panels and tables.

    A border is a set of glyphs that draw a box, and the style to draw them in.
    {!Panel} uses the corners and edges. {!Table} also uses the crossings of its
    column and row rules. *)

(** {1:borders Borders} *)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  vertical : string;  (** The line between two columns. *)
  horizontal : string;
      (** A line across the box: under a header or between rows. *)
  cross : string;  (** The crossing of a horizontal and a vertical line. *)
  top_cross : string;  (** The tee on the top edge, pointing down. *)
  bottom_cross : string;  (** The tee on the bottom edge, pointing up. *)
  left_cross : string;  (** The tee on the left edge, pointing right. *)
  right_cross : string;  (** The tee on the right edge, pointing left. *)
}
(** The type for the glyphs of a border, one glyph per part of the box. Every
    field is used to draw a box, and the fields are independent.

    A glyph may take any number of cells. Each vertical line of the box is as
    wide as its widest glyph, and narrower glyphs are padded with spaces. A
    horizontal line repeats its glyph along its length. If a whole copy does not
    fit at the end, the glyph is cut between two characters and padded with
    spaces.

    Fields may be added to this record. So build a custom border from a
    predefined one, as in [v { (chars single) with top = "=" }]. *)

type t
(** The type for borders. *)

val v : ?style:Style.t -> chars -> t
(** [v ~style chars] is the border drawn with [chars] in [style]. Without
    [style], or with {!Style.none}, it is drawn in the muted colour of the
    formatter's theme ({!Theme.muted}). *)

val chars : t -> chars
(** [chars b] is the glyphs of [b], with each control character replaced by a
    space. *)

val style : t -> Style.t
(** [style b] is the style of [b]'s glyphs. {!Style.none} means the theme's
    muted colour. *)

val with_style : Style.t -> t -> t
(** [with_style s b] is [b] drawn in [s]. *)

val gradient : t -> Color.t list
(** [gradient b] is the colours of [b]'s frame gradient, or [[]] for none. *)

val with_gradient : Color.t list -> t -> t
(** [with_gradient colors b] is [b] with a gradient of [colors] on its frame
    (the four outer edges of a table or a panel), drawn over its style. The
    gradient goes once round the frame and does not move. It runs clockwise from
    the first colour at the top-left corner, through the others, and back to the
    first, so the ends meet without a break ({!Color.gradient}). The lines
    inside a table keep [b]'s style. [[]] gives no gradient. In a live display,
    a theme's motion ({!Theme.type-motion}) is drawn over the gradient. *)

(** {1:predefined Predefined borders} *)

val none : t
(** [none] is the border drawn with empty strings: a box without edges. *)

val simple : t
(** [simple] is the border with only a Unicode light horizontal line, under a
    table's header and between its rows. It has no frame and no line between
    columns. *)

val ascii : t
(** [ascii] is the border drawn with [+], [-] and [|]. *)

val single : t
(** [single] is the border drawn with Unicode light box-drawing lines. *)

val double : t
(** [double] is the border drawn with Unicode double box-drawing lines. *)

val dos : t
(** [dos] is the border of Norton Commander panels in code page 437: a frame of
    Unicode double box-drawing lines, with light lines inside. *)

val rounded : t
(** [rounded] is like {!single} with Unicode arc corners. *)

val heavy : t
(** [heavy] is the border drawn with Unicode heavy box-drawing lines. *)

val hidden : t
(** [hidden] is the border drawn with spaces. It keeps the spacing of a box but
    draws no lines. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a border's corners and edges, for diagnostics. *)
