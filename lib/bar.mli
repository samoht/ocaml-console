(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bars, for progress and for figures.

    A bar is filled in its colour to a fraction of its length, with the rest
    drawn as a track. A {!horizontal} bar shows progress; {!vertical} bars side
    by side ({!View.hcat}) are a sparkline on one row and a histogram on
    several:

    {[
    let sparkline values =
      Console.(View.hcat ~gap:0 (List.map Bar.vertical values))

    let histogram ~height values =
      Console.(
        View.sized ~height (View.hcat ~gap:1 (List.map Bar.vertical values)))
    ]}

    The glyphs of a horizontal bar are those of the formatter's theme
    ({!Theme.get}, {!Theme.val-bar}):

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

val horizontal :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  ?width:int ->
  float ->
  View.t
(** [horizontal ~style ~track ~brackets ~width frac] is a bar [width] cells long
    in [style], filled to [frac] of its length, with [frac] clamped to the range
    0 to 1 and NaN taken as 0. The fill is rounded to the nearest half cell in
    [`Line] glyphs, and to the nearest cell in the others.

    Without [width], the bar grows to its room ({!View.fit}): it takes what the
    other parts of a {!View.hcat} leave it, the formatter's margin under
    {!View.pp}, at least one cell, and ten at its natural size.

    - [track] is the style of the rest (defaults to the muted colour of the
      formatter's theme).
    - [brackets] (defaults to [false]) frames the bar in the [track] style,
      which makes it two cells wider.

    Without [style], the bar takes the {!Theme.palette} of the formatter's
    theme. A palette of several colours is spread along the bar's whole length,
    each filled cell in the colour of its place, so the fill uncovers the
    colours as it grows. A palette of one colour gives that colour.

    The bar is drawn in the theme and {!Fmt.utf_8} of the formatter it is
    printed on. A part that {!View.truncate} or {!View.wrap} keep is drawn as
    those cells of the bar.

    @raise Invalid_argument if [width] is negative. *)

val vertical : ?style:Style.t -> ?track:Style.t -> float -> View.t
(** [vertical ~style ~track frac] is a bar one cell wide, as tall as its room
    ({!View.fit}) and one row at its natural size, filled from the bottom to
    [frac] of its height, with [frac] clamped to the range 0 to 1 and NaN taken
    as 0. Each cell shows the eighths of it that are filled, rounded to the
    nearest eighth of the whole bar: a lower block from U+2581 (one eighth) to
    U+2587 (seven eighths), a full block (U+2588), or a blank when empty. Off
    {!Fmt.utf_8}, and in a theme whose bars are [`Ascii], a cell is a blank, a
    dot ([.]) when filled to less than half, a colon ([:]) to less than seven
    eighths, and a hash ([#]) above.

    [style] and [track] are as for {!horizontal}, a palette of several colours
    spread from the bottom to the top. *)
