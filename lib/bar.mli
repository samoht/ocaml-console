(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal bars, for progress and for figures.

    A bar is [width] cells filled to a fraction of its length: a heavy
    horizontal line (U+2501) in its colour, on a track of the same line in the
    [track] style (defaults to the muted colour of the theme of the formatter it
    is printed on, {!Theme.get}). A half filled cell is the left half of the
    line (U+2578), and the track starts with its right half (U+257A) after a
    whole filled cell, which leaves a gap between the two. [brackets] (defaults
    to [false]) adds a right one eighth block (U+2595) before the bar and a left
    one eighth block (U+258F) after it, in the track style, which makes the bar
    two cells wider.

    On a formatter that is not {!Fmt.utf_8}, the fill is whole cells of [#], the
    track [-] and the brackets [\[] and [\]]. *)

type t
(** The type for bars. *)

val v :
  ?style:Style.t -> ?track:Style.t -> ?brackets:bool -> width:int -> float -> t
(** [v ~width frac] is a [width]-cell bar filled to [frac] of its length,
    rounded to the nearest half cell, in [style] (defaults to the main colour of
    the theme of the formatter it is printed on). [frac] is clamped to the range
    0 to 1, NaN being 0.

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
