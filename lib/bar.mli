(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bars, for progress and for figures.

    A bar is filled in its colour to a fraction of its length. The rest is drawn
    as a track. A {!horizontal} bar shows progress. {!vertical} bars side by
    side ({!View.hcat}) make a {!sparkline} on one row and a {!chart} on several
    rows:

    {[
    let sparkline values =
      Console.(View.hcat ~gap:0 (List.map Bar.vertical values))

    let histogram ~height values =
      Console.(
        View.sized ~height (View.hcat ~gap:1 (List.map Bar.vertical values)))
    ]}

    A horizontal bar takes its glyphs from the formatter's theme ({!Theme.get},
    {!Theme.val-bar}):

    - [`Line] draws a heavy line (U+2501) on a track of light line (U+2500), so
      the fill shows without colour too. A half-filled cell is the left half of
      the heavy line (U+2578). The left bracket is a right one-eighth block
      (U+2595) and the right bracket a left one-eighth block (U+258F).
    - [`Block] fills whole cells with full blocks (U+2588) on a track of light
      shade (U+2591). Its brackets are [\[] and [\]].
    - [`Ascii] fills whole cells with [#] on a track of [-]. Its brackets are
      [\[] and [\]]. On a formatter that is not {!Fmt.utf_8}, every theme draws
      its bars this way.

    A bar has brackets only with [~brackets:true] ({!horizontal}). *)

(** {1:bars Bars} *)

val horizontal :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  ?width:int ->
  float ->
  View.t
(** [horizontal ~style ~track ~brackets ~width frac] is a bar [width] cells long
    in [style], filled to [frac] of its length. [frac] is clamped to the range 0
    to 1, and NaN counts as 0. The fill is rounded to the nearest half cell with
    [`Line] glyphs, and to the nearest cell with the others.

    Without [width], the bar grows to fill its room ({!View.fit}). In a
    {!View.hcat}, it takes the space the other parts leave. Under {!View.pp}, it
    takes the formatter's margin. At its natural size, it is ten cells long.
    With [width], it is [width] cells long, or as long as its room if the room
    is smaller.

    - [track] is the style of the unfilled part (defaults to the muted colour of
      the formatter's theme).
    - [brackets] (defaults to [false]) frames the bar in the [track] style. This
      makes it two cells wider. In a room, the brackets use two of the room's
      cells. They are left out when the room has fewer than three cells.

    Without [style], the bar uses the {!Theme.palette} of the formatter's theme.
    A palette of several colours is spread along the whole bar. Each filled cell
    takes the colour of its position, so the fill reveals the colours as it
    grows. A palette of one colour gives that colour.

    The bar is drawn with the theme and the {!Fmt.utf_8} setting of the
    formatter it is printed on. When {!View.truncate} or {!View.wrap} keep part
    of the bar, that part is drawn as the same cells of the bar.

    @raise Invalid_argument if [width] is negative. *)

val vertical :
  ?style:Style.t ->
  ?track:Style.t ->
  ?peak:float ->
  ?width:int ->
  float ->
  View.t
(** [vertical ~style ~track ~peak ~width frac] is a bar filled from the bottom
    to [frac] of its height. [frac] is clamped to the range 0 to 1, and NaN
    counts as 0. The bar is as tall as its room ({!View.fit}), and one row tall
    at its natural size. Without [width], it is as wide as its room, and one
    cell wide at its natural size, like a column. With [width], it is [width]
    cells wide, but never wider than its room.

    Each cell shows how many eighths of it are filled, rounded to the nearest
    eighth of the whole bar. The cell is a lower block from U+2581 (one eighth)
    to U+2587 (seven eighths), a full block (U+2588), or a blank when empty. On
    a formatter that is not {!Fmt.utf_8}, or in a theme whose bars are [`Ascii],
    a cell is a blank when empty, a dot ([.]) when less than half full, a colon
    ([:]) when less than seven eighths full, and a hash ([#]) otherwise.

    Without [style], each cell takes the colour of the height its fill reaches.
    If the {!Theme.palette} of the formatter's theme has several colours, the
    cells go up that palette. Otherwise they go from the theme's
    {!Theme.succeeded} colour through {!Theme.warning} to {!Theme.failed}. So
    the colours of a sparkline show its values. [track] is the style of an empty
    cell.

    [peak] (defaults to [0.]) marks a height that the bar reached. It is clamped
    like [frac]. The mark is a line in the [track] style, in the cell that
    [peak] reaches: at the bottom of the cell if [peak] fills at most half of
    it, at the top otherwise. In ASCII, the mark is a dash ([-]). The mark shows
    only in a cell that the fill leaves empty. A spectrum with slowly falling
    peaks is a row of such bars:

    {[
    let spectrum ~height bands =
      Console.(
        View.sized ~height
          (View.hcat ~gap:1
             (List.map
                (fun (level, peak) -> Bar.vertical ~width:2 ~peak level)
                bands)))
    ]}

    @raise Invalid_argument if [width] is negative. *)

val sparkline :
  ?style:Style.t -> ?min:float -> ?max:float -> float list -> View.t
(** [sparkline values] is one {!vertical} bar per value, each one cell wide,
    side by side on one row. It stays one row tall in a room of any height. Each
    bar is filled to its value between [min] and [max] (defaults to the lowest
    and the highest of [values]). Without [min], the lowest value fills one
    eighth, so every value shows. In a room narrower than [values], it shows the
    last values that fit. Its minimum width is one cell ({!View.fit}'s
    [min_width]), so a {!Table} can narrow its column like the others. *)

val chart :
  ?height:int ->
  ?scale:float ->
  ?style:(string -> float -> Style.t) ->
  (string * float) list ->
  View.t
(** [chart bars] is a bar chart of [bars], each a [(label, value)] pair. Each
    bar is a {!vertical} bar as wide as the wider of its label and its value.
    The value is above the bar in the muted colour, and the label is under it.
    The bars are one cell apart, on a baseline in the muted colour.

    A value is written as a plain number:
    - a whole value below a million is written in full, such as [12346];
    - another value below ten thousand is written to three significant digits,
      such as [3.14], and as a whole number from a thousand up;
    - any other value is written to three digits in thousands, millions and so
      on, such as [12.3k] for [12345.5].

    A value of [0.] has no number above its bar. A negative or NaN value has an
    empty bar under its number.

    The bars are [height] lines tall. [height] defaults to the room's height
    less three lines (values, baseline and labels), or to 8 when the room has no
    height. Each bar is filled to its value divided by [scale] (defaults to the
    largest value). With [style], a bar is drawn in [style label value]. Without
    it, a bar has the colours of a {!vertical} bar with no style.

    The chart stays inside its room. The bars are at most the room's height less
    three lines, and at least one line. In a room too short for all three lines,
    the values are dropped first, then the baseline, then the labels. A label is
    drawn on one line, and a line wider than the room is cut. *)
