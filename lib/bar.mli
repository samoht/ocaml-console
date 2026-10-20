(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is [width] cells filled to a fraction of its length. Its empty cells
    are the track, drawn as light shade (U+2591) in the [track] style (defaults
    to {!Style.faint}). [brackets] (defaults to [false]) adds a right one eighth
    block (U+2595) before the bar and a left one eighth block (U+258F) after it,
    in the track style, which makes the bar two cells wider.

    On a formatter that is not {!Fmt.utf_8}, the fill is whole cells of [#], the
    track [-] and the brackets [\[] and [\]]. *)

type t
(** The type for bars. *)

val v :
  ?style:Style.t -> ?track:Style.t -> ?brackets:bool -> width:int -> float -> t
(** [v ~width frac] is a [width]-cell bar filled to [frac] of its length,
    rounded to the nearest eighth of a cell: full blocks (U+2588), then the
    block of the partial cell's eighths (U+258F for one to U+2589 for seven), in
    [style] (defaults to the accent colour of the theme of the formatter it is
    printed on, {!Theme.get}). [frac] is clamped to the range 0 to 1, NaN being
    0.

    @raise Invalid_argument if [width] is negative. *)

(** {1:converting Converting} *)

val to_string : ?styled:bool -> t -> string
(** [to_string bar] is [bar] as {!pp} formats it on a formatter of unbounded
    margin, with colours iff [styled] (defaults to [false]). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a bar on one line, [width] cells wide, or two more with
    brackets, and cut with an ellipsis to the formatter's margin. *)

(**/**)

val span :
  utf_8:bool ->
  style:Style.t ->
  track:Style.t ->
  brackets:bool ->
  width:int ->
  float ->
  Span.t
(** [span ~utf_8 ~style ~track ~brackets ~width frac] is the bar of
    [v ~style ~track ~brackets ~width frac] as a span, in ASCII unless [utf_8],
    [frac] in the range 0 to 1. *)

(**/**)
