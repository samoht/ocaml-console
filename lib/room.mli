(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Where and when a view is laid out.

    A room is what a {!View.fit} part is given each time it is laid out: the
    width and the height that it may take, the time it is drawn at, and the
    theme and the character set of the formatter that prints it. *)

type t = Ctx.t
(** The type for rooms. *)

val width : t -> int option
(** [width r] is the number of cells that a view may take on each line. It is
    [None] when nothing bounds the width, as in {!View.hcat} without a room,
    {!View.width} or {!View.to_string}. The view then takes its natural width.
*)

val height : t -> int option
(** [height r] is the number of lines that a view may take, or [None] when
    nothing bounds them. *)

val time : t -> float
(** [time r] is the number of seconds since the {!Display} that draws the view
    started. It is [0.] for a view printed once ({!View.pp}, {!View.to_string},
    {!Display.print}, and the last frame of a display). *)

val theme : t -> Theme.t
(** [theme r] is the theme of the formatter that the view is printed on
    ({!Theme.get}). *)

val utf_8 : t -> bool
(** [utf_8 r] is [true] iff that formatter shows UTF-8 ({!Fmt.utf_8}). Otherwise
    a view draws its glyphs in ASCII. *)

val pp : t Fmt.t
(** [pp] formats the width, the height and the time of a room, for debugging. *)
