(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running a display for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console.driver] reads it through [Console__]. *)

val when_idle : Display.t -> (unit -> unit) -> unit
(** [when_idle display f] calls [f] at once if [display] is neither drawing nor
    writing, and as soon as it is done otherwise. A signal handler, which can
    run at any safe point of the program, calls [display] through it, so that it
    never acts on a frame half drawn or half written. *)

val closed : Display.t -> bool
(** [closed display] is [true] once [display] is closed. *)

val unwritten : Display.t -> bool
(** [unwritten display] is [true] iff a write of [display] is under way or
    waiting, or was cut short and what remains of it waits for the next write.
*)

val reveal : Display.t -> unit
(** [reveal display] shows the cursor of [display] at once, ahead of a close
    that may not end, such as one that runs a view during [exit]. *)

val writing : Display.t -> bool
(** [writing display] is [true] while a write of [display] is under way, such as
    one suspended on an output that takes nothing. *)

val restart : Display.t -> unit
(** [restart display] forgets what [display] drew, so that its next frame is
    drawn afresh on a row of its own with the cursor hidden again, as when a
    program continues from a stop that wrote over the screen. It does nothing
    while {!writing}. *)

val watch :
  Display.t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch display out] is [out], which also tells [display] whether what it
    writes ends its line. A driver passes through it what it takes from a
    formatter that writes where [display] does, so that the next line [display]
    prints without a terminal starts on a line of its own. *)
