(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Displays, with what {!Progress} and a driver need. {!Display} is the public
    interface and documents the values that both modules have. *)

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
      (defaults to [false]; a [Sys_error] is always a write error);
    - [cancelled e] is [true] iff [e] cancels the fiber that draws (defaults to
      [false]). Such an exception is never kept for a later call;
    - [atomic f] writes an update by calling [f] (defaults to calling [f ()]).
      It may stop a cancellation from cutting the update short;
    - [fallback] (defaults to {!View.empty}) is drawn in place of a view that
      raises.

    Without [size], what the display prints once is [once_width ()] cells wide,
    or unbounded without [once_width]. *)

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
    is kept for a later call to [d] that reports failures, as the exception of a
    view is. An exception that is {!Task_core.is_async} or a cancellation is not
    kept. *)

val reporting : t -> ('a -> unit) -> 'a -> unit
(** [reporting d f x] is [f x], where [f] draws what a task reported. A
    cancellation of the reporting fiber is ignored during [f x]. A report is not
    a cancellation point: the fiber meets its cancellation at the next one. *)

val touch : t -> unit
(** [touch d] tells [d] that what the view shows changed. The view is drawn
    again, at most ten times a second. *)

val told : t -> unit
(** [told d] declares that [d] is told ({!touch}) of every change to the tasks
    that its view reads. From then on, a redraw draws a view that reads the
    tasks and no other state only after a change, or when the time moves. *)

val write : ?salvage:string -> t -> string -> unit
(** [write ~salvage d s] writes [s] unchanged, in order with the other writes of
    [d]. If the write is cut short, [salvage] (defaults to [""]) goes out in its
    place with the next write. *)

val reveal : t -> unit
(** [reveal d] shows the cursor of [d] at once, before a close that may never
    end, such as a close that runs a view during [exit]. *)

val unwritten : t -> bool
(** [unwritten d] is [true] iff a write of [d] is under way or waiting, or was
    cut short and the rest of it waits for the next write. *)

val writing : t -> bool
(** [writing d] is [true] while a write of [d] is under way, for example a write
    suspended on an output that accepts nothing. *)

val restart : t -> unit
(** [restart d] forgets what [d] drew, so the next frame is drawn on its own row
    with the cursor hidden again. This is for a program that continues after a
    stop that wrote over the screen. It does nothing while {!writing}. *)

val is_live : t -> bool
(** [is_live d] is [true] iff [d] redraws a region on a terminal. *)

val print_lines : t -> View.t list -> unit
(** [print_lines d lines] prints the lines of [lines], laid out already, as
    {!print} does: each line on a line of its own, cut to the terminal's width
    on a terminal. *)

val closed : t -> bool
(** [closed d] is [true] iff [d] is closed. *)

val width : t -> int
(** [width d] is the terminal's width, read at the last redraw. *)

val height : t -> int
(** [height d] is the terminal's height, read at the last redraw. *)

val look : ?live:bool -> t -> Look.t
(** [look d] is the look that [d] draws in. It is live iff [live] (defaults to
    [false]) and [d] draws in colour. *)

val drawing : t -> bool
(** [drawing d] is [true] while [d] draws or writes. *)

val on_settle : t -> (unit -> unit) -> unit
(** [on_settle d f] makes [d] call [f] at each redraw and when it closes, before
    it draws, so that it shows what other domains sent. *)

val on_close : t -> (unit -> unit) -> unit
(** [on_close d f] makes [d] call [f] when it closes, before its last frame. *)

val on_print : t -> (unit -> unit) -> unit
(** [on_print d f] makes [d] call [f] before it prints anything above the
    region, and when it closes, after the function of {!on_close}. *)

val on_output : t -> (string -> unit) -> unit
(** [on_output d f] gives [f] what {!with_output} prints, instead of printing
    it. *)

val when_idle : t -> (unit -> unit) -> unit
(** [when_idle d f] calls [f] at once if [d] is neither drawing nor writing.
    Otherwise it calls [f] as soon as [d] is done. *)

val watch :
  t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch d out] is [out], but it also tells [d] whether what it writes ends
    its line. So the next line that [d] prints without a terminal starts on a
    line of its own. *)
