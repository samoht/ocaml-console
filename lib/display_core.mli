(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Displays, with what {!Progress} and a driver need. {!Display} is their
    public face and documents the values both have. *)

type t
(** The type for displays. *)

val v :
  ?size:(unit -> int * int) ->
  ?once_width:(unit -> int) ->
  ?write_error:(exn -> bool) ->
  ?cancelled:(exn -> bool) ->
  ?delay:float ->
  ?fallback:View.t ->
  now:(unit -> float) ->
  Format.formatter ->
  View.t ->
  t
(** [v ~size ~once_width ~write_error ~cancelled ~delay ~fallback ~now ppf view]
    is {!Display.v}, which also takes as the end of its output an exception that
    [write_error] says is a write error (defaults to none but [Sys_error]),
    holds back no exception that [cancelled] says is a cancellation (defaults to
    none), and draws [fallback] (defaults to {!View.empty}) in place of a view
    that raises. Without [size], what it prints once is [once_width ()] cells
    wide, and whole without [once_width]. *)

val set_view : t -> View.t -> unit
(** [set_view d view] is {!Display.set_view}. *)

val print : t -> View.t -> unit
(** [print d v] is {!Display.print}. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output d f] is {!Display.with_output}. *)

val redraw : t -> unit
(** [redraw d] is {!Display.redraw}. *)

val close : ?before:(unit -> unit) -> t -> unit
(** [close ~before d] calls [before] (defaults to doing nothing) then is
    {!Display.close}, and what [before] raises waits until [d] closed. *)

val close_quietly : before:(unit -> unit) -> t -> unit
(** [close_quietly ~before d] calls [before] then closes [d] as {!close} does,
    and raises nothing that either raised but what {!is_async} says, which a
    program ending for another reason does not hide. *)

val unwritten : t -> bool
(** [unwritten d] is [true] iff something of [d] waits for {!flush}: a write
    that a signal handler could not do, or what remains of one cut short. *)

val flush : t -> unit
(** [flush d] writes what waits, also once [d] closed. *)

val reveal : t -> unit
(** [reveal d] shows the cursor of [d] at once, ahead of a close that may not
    end, such as one that runs a view during [exit]. *)

val set_cursor : t -> bool -> unit
(** [set_cursor d visible] shows or hides the cursor through the output of [d],
    around a stop of the program on an output with no descriptor. *)

val is_async : exn -> bool
(** [is_async e] is [true] iff [e] is an interrupt or an exhausted heap or
    stack, which can arise anywhere and which no fallback answers. *)

val protect : t -> finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect d ~finally f] is [f ()] followed by [finally ()], whatever [f]
    raises. If both raise, the worse of the two goes on, as itself: an interrupt
    or an exhausted heap or stack, then an error, then a cancellation. *)

val pp : t Fmt.t
(** [pp] is {!Display.pp}. *)

(** {1:internal For progress and drivers} *)

val reporting : t -> (unit -> unit) -> unit
(** [reporting d f] is [f ()], where [f] draws what a task reported, without the
    cancellation of the fiber that reports: a report is no point of
    cancellation, the fiber meets it at its next one, and what was cut short
    goes out with the next write. *)

val touch : t -> unit
(** [touch d] says that what the view shows changed: it is drawn again, at most
    ten times a second. *)

val told : t -> unit
(** [told d] says that [d] is told of every change to the tasks its view reads
    ({!touch}): a redraw draws a view that reads the tasks and no other state
    only after a change or as the time moves. *)

val write : ?salvage:string -> t -> string -> unit
(** [write ~salvage d s] writes [s] as it is, in turn with what [d] writes. If
    this is cut short, [salvage] (defaults to nothing) goes out in its place. *)

val is_live : t -> bool
(** [is_live d] is [true] iff [d] redraws a region on a terminal. *)

val print_lines : t -> View.t list -> unit
(** [print_lines d lines] is {!print} of [lines], each one line that fits. *)

val closed : t -> bool
(** [closed d] is [true] iff [d] closed. *)

val width : t -> int
(** [width d] is the terminal's width, read at the last redraw. *)

val look : ?live:bool -> t -> Look.t
(** [look d] is the look [d] draws in, live iff [live] (defaults to [false]) and
    [d] draws in colour. *)

val on_close : t -> (unit -> unit) -> unit
(** [on_close d f] has [d] call [f] as it closes, before its last frame. *)

val on_output : t -> (string -> unit) -> unit
(** [on_output d f] gives [f] what {!with_output} prints, rather than printing
    it. *)

val when_idle : t -> (unit -> unit) -> unit
(** [when_idle d f] calls [f] at once if [d] is neither drawing nor writing, and
    as soon as it is done otherwise. *)

val watch :
  t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch d out] is [out], which also tells [d] whether what it writes ends its
    line, so that the next line [d] prints without a terminal starts on a line
    of its own. *)
