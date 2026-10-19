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
