(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is [width] cells, filled in its colour to a fraction of its length,
    with the rest drawn as a track. The glyphs are those of the formatter's
    theme ({!Theme.get}, {!Theme.val-bar}):

    - [`Line] draws a heavy horizontal line (U+2501), in which a half filled
      cell is the left half of the line (U+2578). After a whole filled cell the
      track starts with the right half (U+257A), leaving a gap between the two.
      The brackets are a right one eighth block (U+2595) and a left one
      (U+258F).
    - [`Block] fills whole cells with full blocks (U+2588) on a track of light
      shade (U+2591), in brackets [\[] and [\]].
    - [`Ascii] fills whole cells with [#] on a track of [-], in brackets [\[]
      and [\]]. Every theme draws its bars so on a formatter that is not
      {!Fmt.utf_8}. *)

(** {1:bars Bars} *)

type t
(** The type for bars. *)

val v :
  ?style:Style.t -> ?track:Style.t -> ?brackets:bool -> width:int -> float -> t
(** [v ~style ~track ~brackets ~width frac] is a [width]-cell bar in [style],
    filled to [frac] of its length, with [frac] clamped to the range 0 to 1 and
    NaN taken as 0. The fill is rounded to the nearest half cell in [`Line]
    glyphs, and to the nearest cell in the others.

    - [track] is the style of the rest (defaults to the muted colour of the
      formatter's theme).
    - [brackets] (defaults to [false]) frames the bar in the [track] style,
      which makes it two cells wider.

    Without [style], the bar takes the {!Theme.palette} of the formatter's
    theme. A palette of several colours is spread along the bar's whole length,
    each filled cell in the colour of its place, so the fill uncovers the
    colours as it grows. A palette of one colour gives that colour.

    @raise Invalid_argument if [width] is negative. *)

(** {1:converting Converting} *)

val to_span : t -> Span.t
(** [to_span bar] is [bar] as a span, to put in a table cell, a tree or a panel.
    It is drawn when it is printed, as {!pp} draws it, in the formatter's theme
    and {!Fmt.utf_8}, and takes the cells {!pp} gives it. A part that
    {!Span.truncate} or {!Span.wrap} keep is drawn as those cells of the bar. *)

val to_string : ?styled:bool -> t -> string
(** [to_string bar] is [bar] as {!pp} formats it on a formatter of unbounded
    margin, with colours iff [styled] (defaults to [false]). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a bar on one line, [width] cells wide, or two more with
    brackets, cut with an ellipsis to the formatter's margin. *)
