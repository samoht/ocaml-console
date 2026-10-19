(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Time-varying values.

    An animation is a value sampled at an elapsed time: [frame a ~elapsed] is
    what [a] looks like [elapsed] seconds in, as {!Spinner.anim} is. A still
    value is {!const}; {!all} samples several at once so that {!Layout} can join
    them into one frame. *)

type 'a t
(** The type for values that vary with elapsed time. *)

val v : (elapsed:float -> 'a) -> 'a t
(** [v f] is the animation whose frame at [elapsed] is [f ~elapsed]. *)

val const : 'a -> 'a t
(** [const x] is the still animation: every frame is [x]. *)

val frame : 'a t -> elapsed:float -> 'a
(** [frame a ~elapsed] is [a] sampled [elapsed] seconds in. A non-positive
    [elapsed] is the first frame. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] applies [f] to every frame of [a]. *)

val all : 'a t list -> 'a list t
(** [all xs] samples every animation in [xs] at the same elapsed time:
    [frame (all xs) ~elapsed] is [List.map (fun a -> frame a ~elapsed) xs]. It
    is how a {!Layout} combines several animated blocks into one frame. *)
