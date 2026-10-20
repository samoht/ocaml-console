(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated spinners.

    A spinner is a short sequence of frames, such as braille dots or a turning
    ASCII bar, shown beside a task of unknown length. {!Display} animates one
    per running row, ten frames a second, and a {!Theme.t} carries the one a
    display uses. A spinner of one frame, such as {!static}, does not move: a
    program offers it to users who prefer no animation, with [Theme.still],
    which also stops the frames of tables and panels. *)

(** {1:spinners Spinners} *)

type t
(** The type for spinners: non-empty, cyclic sequences of frames. *)

val v : string array -> t
(** [v frames] is a spinner that shows [frames] in order, from the first to the
    last and then from the first again, one frame each [1 / fps] seconds of
    {!frame}. A frame is drawn on one line: a control character in it is a
    space. Changing [frames] after the call does not change the spinner.

    @raise Invalid_argument if [frames] is empty. *)

val braille : t
(** [braille] cycles the ten braille dots [⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏]. *)

val ascii : t
(** [ascii] cycles the four frames [|], [/], [-] and a backslash, for terminals
    or fonts without braille. *)

val static : t
(** [static] is [v [| "*" |]]: a spinner that does not move. *)

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
