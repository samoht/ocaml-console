(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Moving frames: the border cells of a table or a panel, drawn in their
    theme's {!Theme.motion} at a given time. *)

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
    cells recoloured. [lines] is a frame of [border] laid out in [look], whose
    left and right edges take [left] and [right] cells. Its border cells are the
    cells of its first and last lines and of its edges, except, for each
    [(y, first, last)] of [gaps] (defaults to [[]]), cells [first] to [last - 1]
    of line [y].

    The border cells are graded in [border]'s {!Border.gradient}, then drawn as
    the theme's {!Theme.motion} says at [time] if [look] is live. If [look] is
    not live, a sweep keeps its colours at time [0.] and the other motions are
    not drawn.

    The result is [lines] unchanged if the border has no gradient and the theme
    no motion, if an edge is empty, or if there are fewer than two lines or they
    are not all as wide. *)
