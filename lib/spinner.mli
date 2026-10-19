(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated spinners.

    A spinner is a short cyclic sequence of frames -- braille dots, a spinning
    ASCII bar, shown beside a task of unknown length. {!Console.Display}
    animates one per running row, and a {!Theme.t} carries the one a display
    uses. *)

(** {1:spinners Spinners} *)

type t
(** The type for spinners: non-empty, cyclic sequences of frames. *)

val v : string array -> t
(** [v frames] is a spinner cycling [frames] left to right.

    @raise Invalid_argument if [frames] is empty. *)

val braille : t
(** [braille] cycles the ten braille dots [⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏]. *)

val ascii : t
(** [ascii] cycles the four frames [|], [/], [-] and a backslash, for terminals
    or fonts without braille. *)

(** {1:frames Frames} *)

val frame : ?fps:float -> t -> elapsed:float -> string
(** [frame t ~elapsed] is the frame [t] shows [elapsed] seconds in, [fps] frames
    a second (defaults to [10.]): frame [i] of [n] where [i] is the whole part
    of [elapsed * fps] modulo [n]. An [elapsed] that is not positive or not
    finite shows the first frame.

    @raise Invalid_argument if [fps] is not finite and positive. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a spinner for diagnostics, in an unspecified format. *)
