(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running a display for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__]. *)

val when_idle : Display.t -> (unit -> unit) -> unit
(** [when_idle display f] calls [f] at once if [display] is neither drawing nor
    writing, and as soon as it is done otherwise. A signal handler, which runs
    at any safe point of the program, calls [display] through it, so that it
    never works from a frame half drawn or half written. *)

val closed : Display.t -> bool
(** [closed display] is [true] once [display] is closed. *)

val close : Display.t -> before:(unit -> unit) -> unit
(** [close display ~before] calls [before] then is {!Display.close}, and what
    [before] raises waits until [display] closed. *)

val close_quietly : before:(unit -> unit) -> Display.t -> unit
(** [close_quietly ~before display] calls [before] then closes [display] as
    {!Display.close} does, for a program that ends for another reason: it raises
    nothing that either raised but what {!is_async} says, which the program must
    see all the same. *)

val is_async : exn -> bool
(** [is_async e] is [true] iff [e] is an interrupt or an exhausted heap or
    stack, which can arise anywhere and which no fallback answers. *)

val protect : Display.t -> finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect display ~finally f] is [f ()] followed by [finally ()], whatever
    [f] raises. If both raise, the worse of the two goes on, as itself, where
    [Fun.protect] would raise [Fun.Finally_raised]. *)

val watch :
  Display.t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch display out] is [out], which also tells [display] whether what it
    writes ends its line. A driver sends on through it what it takes from a
    formatter that writes where [display] does, so that the next line [display]
    prints without a terminal starts on a line of its own. *)
