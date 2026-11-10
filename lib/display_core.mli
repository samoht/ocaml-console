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
     ppf view] is like {!Display.v}, with four more arguments:
    - [write_error e] is [true] iff [e] is a write error, which ends the output
      (defaults to [false]; a [Sys_error] always is one);
    - [cancelled e] is [true] iff [e] is the cancellation of the fiber that
      draws, which is never kept for a later call (defaults to [false]);
    - [atomic f] writes an update by calling [f] (defaults to [f ()]), and may
      keep a cancellation from cutting it short;
    - [fallback] (defaults to {!View.empty}) is drawn in place of a view that
      raises.

    Without [size], what it prints once is [once_width ()] cells wide, or
    unbounded without [once_width]. *)

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
(** [caught d ~default f] is [f ()], or [default] if [f] raises. The exception
    is kept for a later call to [d] that reports failures, as a view's exception
    is, unless it is {!Task_core.is_async} or a cancellation. *)

val reporting : t -> ('a -> unit) -> 'a -> unit
(** [reporting d f x] is [f x], where [f] draws what a task reported, with the
    cancellation of the reporting fiber ignored. A report is not a cancellation
    point: the fiber meets its cancellation at the next one. *)

val touch : t -> unit
(** [touch d] says that what the view shows changed: it is drawn again, at most
    ten times a second. *)

val told : t -> unit
(** [told d] declares that [d] is told ({!touch}) of every change to the tasks
    its view reads. From then on, a redraw draws a view that reads the tasks and
    no other state only after a change or when the time moves. *)

val write : ?salvage:string -> t -> string -> unit
(** [write ~salvage d s] writes [s] as it is, in order with the other writes of
    [d]. If the write is cut short, [salvage] (defaults to [""]) goes out in its
    place with the next write. *)

val reveal : t -> unit
(** [reveal d] shows the cursor of [d] at once, ahead of a close that may not
    end, such as one that runs a view during [exit]. *)

val unwritten : t -> bool
(** [unwritten d] is [true] iff a write of [d] is under way or waiting, or was
    cut short and what remains of it waits for the next write. *)

val writing : t -> bool
(** [writing d] is [true] while a write of [d] is under way, such as one
    suspended on an output that takes nothing. *)

val restart : t -> unit
(** [restart d] forgets what [d] drew, so that the next frame is drawn on a row
    of its own with the cursor hidden again, as when a program continues from a
    stop that wrote over the screen. It does nothing while {!writing}. *)

val is_live : t -> bool
(** [is_live d] is [true] iff [d] redraws a region on a terminal. *)

val print_lines : t -> View.t list -> unit
(** [print_lines d lines] prints [lines] as {!print} does. Each of [lines] must
    be a single line that fits the terminal. *)

val closed : t -> bool
(** [closed d] is [true] iff [d] is closed. *)

val width : t -> int
(** [width d] is the terminal's width, read at the last redraw. *)

val height : t -> int
(** [height d] is the terminal's height, read at the last redraw. *)

val look : ?live:bool -> t -> Look.t
(** [look d] is the look [d] draws in, live iff [live] (defaults to [false]) and
    [d] draws in colour. *)

val drawing : t -> bool
(** [drawing d] is [true] while [d] draws or writes. *)

val on_settle : t -> (unit -> unit) -> unit
(** [on_settle d f] has [d] call [f] at each redraw and as it closes, before it
    draws, so that it shows what other domains sent. *)

val on_close : t -> (unit -> unit) -> unit
(** [on_close d f] has [d] call [f] as it closes, before its last frame. *)

val on_print : t -> (unit -> unit) -> unit
(** [on_print d f] has [d] call [f] before it prints anything above the region,
    and as it closes, after {!on_close}'s function. *)

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
