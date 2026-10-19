(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is a {!Span.t} exactly [width] cells wide. Its empty cells are the
    track, drawn ["░"] in the [track] style (defaults to {!Style.faint}).
    [brackets] (defaults to [false]) adds a ["▕"] before the bar and a ["▏"]
    after it, in the track style, which makes the bar two cells wider. *)

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
    [scale]: solid ["█"] for the cells of [median], then shaded ["▒"] up to the
    cells of [p95], both in [style] (default [Style.fg Color.cyan]). A value
    that is not positive takes no cell and a positive one at least one; a [p95]
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
