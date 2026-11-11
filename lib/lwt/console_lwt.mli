(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocsigen/lwt}Lwt}.

    {!run} shows the tasks of a program ({!Console.Task}) on the terminal as
    [Console_eio.run] does, but with an Lwt promise instead of a fiber. The
    documentation of [Console_eio] applies, except where this page says
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

val run :
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
    [ppf] until that promise resolves. Unless [f] already ended [root], [root]
    succeeds when the promise resolves and fails when it is rejected. If the
    rejection is [Lwt.Canceled], [root] is cancelled instead. [run] does not
    raise. Its promise is rejected with the exception of [f] after the display
    closes.

    The arguments are those of [Console_eio.run].

    [Lwt.cancel] on [run]'s promise cancels the promise that [run] waits on: the
    promise of [f], or that of the display's last writes. Then the display
    closes as for any rejection. If a view raises in a redraw, the promise of
    [f] is cancelled, if it can be, and [run] is rejected with the view's
    exception.

    A [run] called while another run is active, nested or not, joins it as in
    [Console_eio]. *)

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
  (** [run parent label f] is the promise of [f task], where {!Console.Task.run}
      starts [task]. The task succeeds when the promise resolves and fails when
      it is rejected. If the rejection is [Lwt.Canceled], the task is cancelled
      instead. [task] is current in [f] and in the promises that [f] binds.
      [run] does not raise: an exception from [f] rejects the promise.

      {!Console.Task.within} also accepts a function that returns a promise, and
      makes the task current in the promises that this function binds. *)
end

(** {1:lwt Lwt}

    The current task and the run are bound with [Lwt.with_value]. So the
    callbacks of a promise see the task and the run of the code that made the
    promise. A signal that the program handles, with [Sys.signal] or
    [Lwt_unix.on_signal], is left to the program, as in [Console_eio].

    {1:limits Limitations}

    - Call [run] on the thread that runs Lwt. Another thread or domain sees no
      task. A message logged in another thread, such as one of
      [Lwt_preemptive.detach], goes to the reporter installed before [run]. A
      message from another domain reaches the run outside any task.
    - There is no deterministic mode. With [CONSOLE_DETERMINISTIC_CLOCK] or
      [CONSOLE_CAST] set, [run] still uses real time, and a [CONSOLE_CAST]
      recording has real times.

    - There is no prompt ([Console_eio.Prompt]). *)
