(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal progress bars.

    A bar fills left to right to a percentage. {!render} draws a determinate bar
    and {!at} a moving, indeterminate one -- the bar analogue of {!Spinner}. The
    look (blocky, smooth or a rainbow gradient) and the fill colour come from a
    {!Theme.t}, or are given explicitly. {!Console.Display} draws a row's bar
    through this module.

    {!v}, {!spread} and {!stacked} draw a bar as a {!Span.t}, for a screen that
    lays bars out beside other spans: a fraction to the eighth of a cell, a
    median and a 95th percentile on one bar, and the parts of a whole. *)

val render :
  ?theme:Theme.t ->
  ?style:Theme.bar ->
  ?color:Color.t ->
  ?styled:bool ->
  width:int ->
  pct:int ->
  unit ->
  string
(** [render ~width ~pct ()] is a [width]-cell bar filled to [pct] (clamped to
    the range 0 to 100). The look comes from the style argument if given, else
    [theme]'s, else [`Smooth]; the fill from the color argument if given, else
    [theme]'s accent, else {!Color.cyan}. [styled] (default [true]) is whether
    the bar carries ANSI: [false] renders the same glyphs with no SGR at all,
    for a target whose [Fmt.style_renderer] is [`None].

    @raise Invalid_argument if [width] is negative. *)

val at :
  ?theme:Theme.t ->
  ?style:Theme.bar ->
  ?color:Color.t ->
  ?styled:bool ->
  width:int ->
  elapsed:float ->
  unit ->
  string
(** [at ~width ~elapsed ()] is a moving, indeterminate bar [elapsed] seconds
    into the animation: the fill sweeps up and back over a two-second period,
    for work whose progress is unknown. Style, colour and [styled] resolve as in
    {!render}. Non-finite or negative elapsed time renders the initial frame.

    @raise Invalid_argument if [width] is negative. *)

val anim :
  ?theme:Theme.t ->
  ?style:Theme.bar ->
  ?color:Color.t ->
  ?styled:bool ->
  width:int ->
  unit ->
  string Anim.t
(** [anim ~width ()] is the moving bar as an {!Anim.t}: its frame at [elapsed]
    is [at ~width ~elapsed ()]. Style, colour and [styled] resolve as in
    {!render}. *)

(** {1:spans Bars as spans}

    Each bar below is exactly [width] cells wide. Its empty cells are the track,
    drawn ["░"] in the [track] style (default {!Style.faint}). [brackets]
    (default [false]) adds a ["▕"] before the bar and a ["▏"] after it, in the
    track style, which makes the bar two cells wider. A part of the bar that
    takes no cell adds nothing to the span, no style either. *)

val v :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  width:int ->
  float ->
  Span.t
(** [v ~width frac] is a [width]-cell bar filled to [frac] of its length,
    rounded to the nearest eighth of a cell: full cells ["█"], then the partial
    cell's eighth glyph (["▏"] for one eighth to ["▉"] for seven), in [style]
    (default [Style.fg Color.cyan]). [frac] is clamped to the range 0 to 1; NaN
    is 0.

    @raise Invalid_argument if [width] is negative. *)

val cells : width:int -> scale:float -> float -> int
(** [cells ~width ~scale x] is the number of whole cells, out of [width], that
    [x] takes on a bar whose full length is [scale], to the nearest cell. It is
    [0] when [x] or [scale] is not positive, and at least [1] otherwise, so a
    small value stays visible; it is at most [width]. *)

val spread :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  width:int ->
  scale:float ->
  float ->
  float ->
  Span.t
(** [spread ~width ~scale median p95] draws two levels on one bar of full length
    [scale]: solid ["█"] for {!cells} of [median], then shaded ["▒"] up to the
    cells of [p95], both in [style] (default [Style.fg Color.cyan]). A [p95]
    below [median] adds no shaded cell.

    @raise Invalid_argument if [width] is negative. *)

val stacked :
  ?track:Style.t ->
  ?brackets:bool ->
  width:int ->
  (int * Style.t) list ->
  Span.t
(** [stacked ~width parts] splits [width] cells among [parts], in order, each a
    count and the style of its full cells ["█"]. Each part takes its share of
    the cells by the largest remainder method: the whole part of its exact
    share, then one more cell for each part with the largest fractional
    remainders, the earlier part first on a tie, until the cells add up to
    [width]. Parts that sum to zero give an empty track.

    @raise Invalid_argument if [width] or a count is negative. *)
