(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated frames. This module recolours the border cells of a table or a
    panel with the theme's {!Theme.motion} at a given time. *)

val frame :
  Look.t ->
  time:float ->
  Border.t ->
  left:int ->
  right:int ->
  ?gaps:(int * int * int) list ->
  View_core.t list ->
  View_core.t list
(** [frame look ~time border ~left ~right lines] is [lines] with its border
    cells recoloured. [lines] is a frame of [border] laid out in [look]. Its
    left edge is [left] cells wide and its right edge [right] cells wide. The
    border cells are the cells of the first line, the last line, and the two
    edges. For each [(y, first, last)] in [gaps] (defaults to [[]]), cells
    [first] to [last - 1] of line [y] are not border cells.

    The border cells first get [border]'s {!Border.gradient}. Then, if [look] is
    live, they are drawn with the theme's {!Theme.motion} at [time]. If [look]
    is not live, a sweep is drawn at time [0.] and other motions are not drawn.

    The result is [lines] unchanged if the border has no gradient and the theme
    has no motion, if an edge is empty, if there are fewer than two lines, or if
    the lines differ in width. *)
