(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocsigen/lwt}Lwt}.

    {!run} shows the tasks of a program ({!Console.Task}) on the terminal as
    [Console_eio.run] does, with an Lwt promise in place of a fiber. The
    documentation of [Console_eio] holds for it except where this one says
    otherwise.

    {[
    let build root =
      Console_lwt.Task.run root "compile" (fun step ->
          Console.Task.set_items step ~count:1 ~total:2;
          let open Lwt.Syntax in
          let+ () = Lwt.pause () in
          Console.Task.set_items step ~count:2 ~total:2)

    let () = Lwt_main.run (Console_lwt.run ~progress:`Plain build)
    ]} *)

(** Where a run reads the time. *)
module Clock : sig
  type t
  (** The type for clocks. *)

  val v : now:(unit -> float) -> sleep:(float -> unit Lwt.t) -> t
  (** [v ~now ~sleep] is a clock that reads the time, in seconds, on [now], and
      waits between two redraws with [sleep]. A test passes a [now] it moves by
      hand, and a [sleep] that never resolves to redraw only when it asks. [now]
      is called from any domain. *)

  val system : t
  (** [system] reads the monotonic clock of the system and waits with
      [Lwt_unix.sleep]. Under [CONSOLE_DETERMINISTIC_CLOCK] or [CONSOLE_CAST],
      its time stays at zero, since Lwt has no virtual clock that the program's
      sleeps would share. *)
end

val run :
  ?clock:Clock.t ->
  ?ppf:Format.formatter ->
  ?progress:Console.Progress.t ->
  ?layout:Console.Progress.Layout.t ->
  ?label:string ->
  ?view:(Console.Task.t -> Console.View.t) ->
  ?keep_ended:bool ->
  ?commit:(Console.Task.t -> Console.View.t option) ->
  ?plain:(Console.Task.Event.t -> Console.View.t option) ->
  ?delay:float ->
  (Console.Task.t -> 'a Lwt.t) ->
  'a Lwt.t
(** [run f] is the promise of [f root], with the work under [root] shown on
    [ppf] until that promise resolves. Unless [f] ended it, [root] succeeds when
    the promise resolves and fails when it is rejected, or is cancelled if the
    rejection is [Lwt.Canceled]. [run] does not raise. Its promise is rejected
    with the exception of [f] once the display has closed.

    [clock] defaults to {!Clock.system}; the other arguments are those of
    [Console_eio.run].

    [Lwt.cancel] on [run]'s promise cancels the promise it waits on, that of [f]
    or of the display's last writes, then the display closes as for any
    rejection. A view that raises in a redraw cancels the promise of [f], and
    [run] is rejected with the view's exception.

    Call [run] in the domain and the thread that run [Lwt_main.run]. A [run]
    while another runs, nested or not, joins it as in [Console_eio]. *)

val with_output : (Format.formatter -> 'a) -> 'a
(** [with_output f] is {!Console.Display.with_output} on the display of the run,
    as [Console_eio.with_output]. *)

val print : Console.View.t -> unit
(** [print v] is {!Console.Display.print} on the display of the run, as
    [Console_eio.print]. *)

val redraw : unit -> unit
(** [redraw ()] is {!Console.Display.redraw} on the display of the run, as
    [Console_eio.redraw]. *)

(** Tasks that end with a promise. *)
module Task : sig
  val run :
    ?key:string ->
    ?log_lines:int ->
    ?transient:bool ->
    Console.Task.t ->
    string ->
    (Console.Task.t -> 'a Lwt.t) ->
    'a Lwt.t
  (** [run parent label f] is the promise of [f task], for a [task] that
      {!Console.Task.run} starts. The task succeeds when the promise resolves
      and fails when it is rejected, or is cancelled if the rejection is
      [Lwt.Canceled]. [task] is current in [f] and in the promises that [f]
      binds. [run] does not raise, and what [f] raises rejects the promise.

      {!Console.Task.within} also takes a function that returns a promise, and
      makes the task current in the promises it binds. *)
end

(** {1:lwt Lwt}

    The current task and the run are bound with [Lwt.with_value], so that the
    callbacks of a promise see those of the code that made the promise. Another
    thread or domain sees neither. A message logged in another thread, such as
    one of [Lwt_preemptive.detach], goes to the reporter installed before [run].
    A message from another domain reaches the run outside any task.

    A signal handler that the program installs with [Sys.signal] runs in the
    thread of Lwt, so a message it logs reaches the display, unlike under
    [Console_eio]. Handlers installed with [Lwt_unix.on_signal] run as any
    callback does. *)
