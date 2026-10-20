(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is a {!Span.t} exactly [width] cells wide. Its empty cells are the
    track, drawn as light shade (U+2591) in the [track] style (defaults to
    {!Style.faint}). [brackets] (defaults to [false]) adds a right one eighth
    block (U+2595) before the bar and a left one eighth block (U+258F) after it,
    in the track style, which makes the bar two cells wider. *)

val v :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  width:int ->
  float ->
  Span.t
(** [v ~width frac] is a [width]-cell bar filled to [frac] of its length,
    rounded to the nearest eighth of a cell: full blocks (U+2588), then the
    block of the partial cell's eighths (U+258F for one to U+2589 for seven), in
    [style] (defaults to [Style.fg (Theme.accent Theme.default)]). [frac] is
    clamped to the range 0 to 1, NaN being 0. The invariant
    [Span.width (v ~brackets ~width frac) = width + (if brackets then 2 else 0)]
    holds.

    @raise Invalid_argument if [width] is negative. *)
