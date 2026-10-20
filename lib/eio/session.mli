(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The display that runs, and what goes on around it: the program's output
    above its region, {!Logs} messages, the redraws, and its closing however the
    program ends. *)

val find : unit -> Console.Display.t option
(** [find ()] is the display of the calling fiber, if any. *)

val scope : Console.Display.scope
(** [scope] is the scope in which the current task is that of the innermost
    {!Console.Display.with_task} of the calling fiber or of the fibers it forked
    from. A fiber cancelled in it cancels its task. *)

val claim : unit -> unit
(** [claim ()] makes the caller the one that opens a display, until {!release}.

    @raise Invalid_argument if a display already runs. *)

val release : unit -> unit
(** [release ()] ends a {!claim}. *)

val with_terminal : (unit -> 'a) -> 'a
(** [with_terminal f] is [f ()] with the region of the calling fiber's display,
    if any, off the screen, and what [f] writes on the standard formatters going
    straight out. *)

val run :
  ?cursor:(bool -> unit) ->
  sleep:(float -> unit) ->
  intercept:Format.formatter list ->
  Console.Display.t ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~sleep ~intercept display f] is [f display], with [display] the calling
    fiber's and the program's one, redrawn ten times a second, waiting with
    [sleep], {!Logs} messages and each whole line written on the formatters of
    [intercept] printed above its region, and closed when [f] returns, raises,
    is cancelled, the program exits or a signal ends it ({!Interrupt}). While
    SIGTSTP stops the program, the region is off the screen and the cursor shown
    with [cursor true] (default does nothing); once continued, [cursor false]
    hides it and the region is drawn again. A {!claim} must be held; it is
    released. *)
