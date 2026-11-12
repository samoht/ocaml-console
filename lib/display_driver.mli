(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running a display for a scheduler.

    This module is internal to console. [Console] does not expose it;
    [console.driver] reads it through [Console__]. *)

val when_idle : Display.t -> (unit -> unit) -> unit
(** [when_idle display f] calls [f] at once if [display] is neither drawing nor
    writing. Otherwise it calls [f] as soon as [display] is done. A signal
    handler can run at any safe point of the program, so it calls [display]
    through [when_idle]. Then it never acts on a frame that is half drawn or
    half written. *)

val closed : Display.t -> bool
(** [closed display] is [true] once [display] is closed. *)

val unwritten : Display.t -> bool
(** [unwritten display] is [true] iff a write of [display] is under way or
    waiting, or was cut short and the rest of it waits for the next write. *)

val reveal : Display.t -> unit
(** [reveal display] shows the cursor of [display] at once, before a close that
    may never end, such as a close that runs a view during [exit]. *)

val writing : Display.t -> bool
(** [writing display] is [true] while a write of [display] is under way, for
    example a write suspended on an output that accepts nothing. *)

val restart : Display.t -> unit
(** [restart display] forgets what [display] drew, so its next frame is drawn
    from scratch, on its own row, with the cursor hidden again. This is for a
    program that continues after a stop that wrote over the screen. It does
    nothing while {!writing}. *)

val watch :
  Display.t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch display out] is [out], but it also tells [display] whether what it
    writes ends its line. A driver passes through it the output that it takes
    from a formatter that writes where [display] writes. So the next line that
    [display] prints without a terminal starts on a line of its own. *)
