(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Spinners, for work of unknown length.

    A spinner is a short cycle of frames, such as braille dots or a turning
    ASCII bar. Its {!view} can go in any view. A {!Display} advances it at each
    frame. A view printed once shows the first frame. The row of each running
    task shows the spinner of its theme ({!Theme.val-spinner}).

    A spinner with one frame does not move. A program can offer this to users
    who prefer no animation, with {!Theme.still}. {!Theme.still} also stops the
    animated frames of tables and panels. *)

val view : ?frames:string list -> unit -> View.t
(** [view ()] shows [frames] in turn, from first to last, then again from the
    first. It shows ten frames per second of the time it is drawn at
    ({!View.fit}), starting with the first frame at time [0.]. [frames] defaults
    to the spinner of the theme it is printed in. A frame is drawn on one line,
    with each control character drawn as a space. On a formatter that is not
    {!Fmt.utf_8}, a spinner whose frames are not all ASCII is drawn as {!ascii}.
    Every frame takes the width of the widest frame, so the view does not change
    size as it turns. A room narrower than that cuts every frame to the room.

    @raise Invalid_argument if [frames] is empty. *)

val braille : string list
(** [braille] is the ten braille patterns U+280B, U+2819, U+2839, U+2838,
    U+283C, U+2834, U+2826, U+2827, U+2807 and U+280F. *)

val ascii : string list
(** [ascii] is the four frames [|], [/], [-] and a backslash, for terminals or
    fonts without braille. *)
