(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The running display and what surrounds it: the program's output and {!Logs}
    messages, the redraws, and the closing of the display however the program
    ends. *)

type run = { display : Console.Display.t; root : Console.Task.t; json : bool }
(** The type for what {!run} runs: a display, the root of the work it shows, and
    whether it shows it as JSON events. *)

val find : unit -> Console.Task.t option
(** [find ()] is the root of the run of the calling fiber, if it has one whose
    display is not closing. *)

val display : unit -> Console.Display.t option
(** [display ()] is the display of the run of the calling fiber, if it has one
    that is not closing. It is [None] outside Eio. *)

val given : Console.Task.t -> Console.Task.t
(** [given root] is the task that a run inside another is given: the task of
    [root]'s work current where it is called, or [root]. *)

val scope : Task_driver.scope
(** [scope] is the scope in which the current task is that of the innermost
    {!Console.Task.run} or {!Console.Task.within} of the calling fiber or of the
    fibers it forked from. A fiber cancelled in a {!Console.Task.run} cancels
    its task. *)

val cancelling : exn -> bool
(** [cancelling e] is [true] iff [e] is the cancellation of the fiber that calls
    it. A [Cancelled] elsewhere is an exception like any other. *)

val atomic : (unit -> unit) -> unit
(** [atomic f] is [f ()], where [f] writes an update. In a cancelled fiber too,
    the update is written in full, until a deadline at most. All such writes
    share that deadline: a second after the first of them, until one goes out.
*)

val write_error : exn -> bool
(** [write_error e] is [true] iff [e] is a write error of an Eio flow, such as a
    closed pipe. *)

val join : (Console.Task.t -> 'a) -> 'a option
(** [join f] is [Some (f task)] if a run whose display is not closing runs in
    the program. [f] runs as part of that run, as the calling fiber's, with
    [task] {!given} by its root, and the display closes only once [f] has
    returned too. It is [None] otherwise. *)

val await_release : unit -> unit
(** [await_release ()] waits until the display that {!run} runs is gone, if it
    is closing. *)

val claim : unit -> bool
(** [claim ()] makes the caller the one that opens a display, until {!release}.
    It is [false] if another caller holds that claim. *)

val release : unit -> unit
(** [release ()] ends a {!claim}. *)

val with_terminal : (unit -> 'a) -> 'a
(** [with_terminal f] is [f ()] with the region of the calling fiber's display,
    if any, off the screen, and what [f] and the fibers it forks write on the
    standard formatters going straight out. What other fibers write meanwhile is
    taken as usual. *)

val run :
  ?cursor:(bool -> unit) ->
  ?flow:bool ->
  ?terminal:Unix.file_descr ->
  sleep:(float -> unit) ->
  intercept:Format.formatter list ->
  run ->
  (Console.Task.t -> 'a) ->
  'a
(** [run ~sleep ~intercept r f] is [f r.root], with [r] as the run of the
    calling fiber and of the program:
    - the display is redrawn ten times a second, waiting with [sleep];
    - {!Logs} messages and what is written on the formatters of [intercept] go,
      a line at a time, to the task current where they are written, or to the
      root outside a task, and a task's unended line is ended when the task
      ends;
    - the root succeeds when [f] returns, fails when [f] raises and is cancelled
      when its fiber is, unless [f] ended it;
    - the display closes when [f] returns, raises or is cancelled, when the
      program exits, and when a signal ends it ({!Interrupt});
    - while SIGTSTP stops the program, the region is off the screen and the
      cursor is shown with [cursor true] (defaults to doing nothing); once the
      program continues, [cursor false] hides it and the region is drawn again;
    - with [flow] (defaults to [false]) the output is an Eio flow, so a signal
      handler that runs outside any fiber leaves its writes to a fiber of the
      run;
    - a signal or an exit that cannot wait for the display, from another domain
      or thread or during a write that does not end, writes
      {!Interrupt.last_resort} on [terminal], a non-blocking descriptor on the
      display's terminal, if given.

    A {!claim} must be held; [run] releases it. *)
