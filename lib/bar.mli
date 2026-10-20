(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is [width] cells filled to a fraction of its length in its colour, on
    a track in the [track] style (defaults to the muted colour of the theme of
    the formatter it is printed on, {!Theme.get}), in the glyphs of that theme
    ({!Theme.val-bar}):

    - [`Line] draws a heavy horizontal line (U+2501), a half filled cell being
      the left half of the line (U+2578) and the track starting with its right
      half (U+257A) after a whole filled cell, which leaves a gap between the
      two; brackets are a right one eighth block (U+2595) and a left one
      (U+258F);
    - [`Block] fills whole cells with full blocks (U+2588) on a track of light
      shade (U+2591), in brackets [\[] and [\]];
    - [`Ascii] fills whole cells with [#] on a track of [-], in brackets [\[]
      and [\]]. Every theme draws its bars so on a formatter that is not
      {!Fmt.utf_8}.

    [brackets] (defaults to [false]) frames the bar in the track style, which
    makes it two cells wider. *)

(** {1:bars Bars} *)

type t
(** The type for bars. *)

val v :
  ?style:Style.t -> ?track:Style.t -> ?brackets:bool -> width:int -> float -> t
(** [v ~style ~width frac] is a [width]-cell bar filled to [frac] of its length,
    rounded to the nearest half cell in [`Line] glyphs and to the nearest cell
    in the others, in [style]. Without [style], the bar takes the
    {!Theme.palette} of the theme of the formatter it is printed on: a palette
    of several colours is spread along its whole length, each filled cell in the
    colour of its place, so the fill uncovers the colours as it grows; a palette
    of one colour gives the main colour of the theme. [frac] is clamped to the
    range 0 to 1, NaN being 0.

    @raise Invalid_argument if [width] is negative. *)

(** {1:converting Converting} *)

val to_span : t -> Span.t
(** [to_span bar] is [bar] as a span, to put in a table cell, a tree or a panel:
    it is drawn when it is printed, in the theme and the {!Fmt.utf_8} of the
    formatter it is printed on, as {!pp} draws it, and takes the cells {!pp}
    gives it. A part that {!Span.truncate} or {!Span.wrap} keep is drawn as
    those cells of the bar. *)

val to_string : ?styled:bool -> t -> string
(** [to_string bar] is [bar] as {!pp} formats it on a formatter of unbounded
    margin, with colours iff [styled] (defaults to [false]). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a bar on one line, [width] cells wide, or two more with
    brackets, and cut with an ellipsis to the formatter's margin. *)
