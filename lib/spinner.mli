(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated spinners.

    A spinner is a short cyclic sequence of frames -- braille dots, a spinning
    ASCII bar -- shown beside an indeterminate task. {!Console.Display} animates
    one per running row, and a {!Theme.t} carries the one a display uses. *)

(** {1:spinners Spinners} *)

type t
(** The type for spinners: non-empty, cyclic sequences of frames. *)

val v : string array -> t
(** [v frames] is a spinner cycling [frames] left to right.

    @raise Invalid_argument if [frames] is empty. *)

val frames : t -> string array
(** [frames t] are [t]'s frames, in order. *)

val braille : t
(** [braille] cycles the ten braille dots [⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏]. *)

val ascii : t
(** [ascii] cycles the four frames [|], [/], [-] and a backslash, for terminals
    or fonts without braille. *)

(** {1:anim Animation} *)

val anim : ?fps:float -> t -> string Anim.t
(** [anim t] is [t] as an {!Anim.t}: its frames in turn, [fps] a second
    (defaults to [10.]), starting from the first at [0.]. A time that is not
    finite shows the first frame.

    @raise Invalid_argument if [fps] is not finite and positive. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a spinner for diagnostics, in an unspecified format. *)
