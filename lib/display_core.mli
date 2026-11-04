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
  ?atomic:((unit -> unit) -> unit) ->
  ?delay:float ->
  ?fallback:View.t ->
  now:(unit -> float) ->
  Format.formatter ->
  View.t ->
  t
(** [v ~size ~once_width ~write_error ~cancelled ~atomic ~delay ~fallback ~now
     ppf view] is {!Display.v}, which also takes as the end of its output an
    exception that [write_error] says is a write error (defaults to none but
    [Sys_error]), holds back no exception that [cancelled] says is the
    cancellation of the fiber that draws (defaults to none), writes each update
    through [atomic] (defaults to calling it), which may keep a cancellation
    from cutting it, and draws [fallback] (defaults to {!View.empty}) in place
    of a view that raises. Without [size], what it prints once is
    [once_width ()] cells wide, and whole without [once_width]. *)

val set_view : t -> View.t -> unit
(** [set_view d view] is {!Display.set_view}. *)

val print : t -> View.t -> unit
(** [print d v] is {!Display.print}. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output d f] is {!Display.with_output}. *)

val redraw : t -> unit
(** [redraw d] is {!Display.redraw}. *)

val close : t -> unit
(** [close d] is {!Display.close}. *)

val pp : t Fmt.t
(** [pp] is {!Display.pp}. *)

(** {1:internal For progress and drivers} *)

val caught : t -> default:'a -> (unit -> 'a) -> 'a
(** [caught d ~default f] is [f ()], or [default] once [f] raised: what it
    raised waits for a call to [d] that asks, as a view's exception does, unless
    it is {!Task_core.is_async} or a cancellation. *)

val reporting : t -> ('a -> unit) -> 'a -> unit
(** [reporting d f x] is [f x], where [f] draws what a task reported, without
    the cancellation of the fiber that reports: a report is no point of
    cancellation, and the fiber meets it at its next one. *)

val touch : t -> unit
(** [touch d] says that what the view shows changed: it is drawn again, at most
    ten times a second. *)

val told : t -> unit
(** [told d] says that [d] is told of every change to the tasks its view reads
    ({!touch}): a redraw draws a view that reads the tasks and no other state
    only after a change or as the time moves. *)

val write : ?salvage:string -> t -> string -> unit
(** [write ~salvage d s] writes [s] as it is, in turn with what [d] writes. If
    this is cut short, [salvage] (defaults to nothing) goes out in its place
    with the next write. *)

val reveal : t -> unit
(** [reveal d] shows the cursor of [d] at once, ahead of a close that may not
    end, such as one that runs a view during [exit]. *)

val unwritten : t -> bool
(** [unwritten d] is [true] iff a write of [d] is under way, waits, or was cut
    short and what remains of it waits for the next. *)

val writing : t -> bool
(** [writing d] is [true] while a write of [d] is under way, such as one
    suspended on an output that takes nothing. *)

val restart : t -> unit
(** [restart d] forgets what [d] drew, so that the next frame is drawn from a
    row of its own with the cursor hidden again: what is on the screen is not
    known, as once a program continues from a stop that wrote over it. It does
    nothing while {!writing}. *)

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
