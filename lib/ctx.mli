(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Where and when a view is laid out. {!Room} exposes this type. *)

type t
(** The type for rooms: the look a view is drawn in, the width and the height it
    may take, if bounded, and the seconds since the display that draws it
    started, [0.] for a view printed once. *)

val look : t -> Look.t
(** [look c] is the look a view is drawn in. *)

val width : t -> int option
(** [width c] is {!Room.width}. *)

val height : t -> int option
(** [height c] is {!Room.height}. *)

val time : t -> float
(** [time c] is the seconds since the display that draws the view started, [0.]
    for a view printed once. *)

val v : ?width:int -> ?height:int -> ?time:float -> Look.t -> t
(** [v ~width ~height ~time look] is the room in [look] bounded by [width] and
    [height] (defaults to unbounded) at [time] (defaults to [0.]). *)

val default : t
(** [default] is [v Look.default], the room a view is measured in. *)

val natural : t -> t
(** [natural c] is [c] unbounded. *)

val with_room : width:int option -> height:int option -> t -> t
(** [with_room ~width ~height c] is [c] bounded by [width] and [height]. *)

val with_width : int option -> t -> t
(** [with_width w c] is [c] bounded by the width [w] and not by a height. *)

val still : t -> t
(** [still c] is [c] at time [0.]. *)

val same_room : t -> t -> bool
(** [same_room c0 c1] is [true] iff [c0] and [c1] have equal looks and bounds,
    whatever their times. *)
