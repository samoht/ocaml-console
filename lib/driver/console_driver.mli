(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running console on a scheduler.

    A backend gives {!Make} what it needs from a scheduler and gets {!T.run},
    {!T.print}, {!T.with_output} and {!T.redraw}, all in direct style. [run]
    does the rest: it finds the terminal, shows the work, takes the program's
    output and its [Logs] messages, redraws, and gives the terminal back.

    This library is [console.driver], on which [console-eio] and [console-lwt]
    build. *)

(** What a scheduler gives console. A {e fiber} is whatever the scheduler runs
    concurrently in a domain. The driver calls {!S.notify} from any thread,
    domain or signal handler, and every other function only on the first thread
    of each domain that starts a [run] or makes a task current there. A
    scheduler without cancellation takes [first a _] as [a ()], [protect f] as
    [f ()], and [check ()] as doing nothing. *)
module type S = sig
  (** {1:clock Time} *)

  val clock : unit -> unit -> float
  (** [clock () ()] is the time that the display shows and records, in seconds
      from any origin. The driver applies [clock ()] as a display opens, in a
      fiber of its run, and calls the function it returns from any domain. *)

  val tick : float -> unit
  (** [tick d] waits [d] seconds on that time between two redraws. *)

  val sleep : float -> unit
  (** [sleep d] waits [d] real seconds. *)

  (** {1:local Fiber-local values} *)

  type 'a key
  (** The type for values bound in a fiber and in the fibers it starts. *)

  val key : unit -> 'a key
  (** [key ()] is a new key. *)

  val get : 'a key -> 'a option
  (** [get k] is the value bound to [k] in the calling fiber, and [None] outside
      any fiber. *)

  val with_binding : 'a key -> 'a -> (unit -> 'b) -> 'b
  (** [with_binding k v f] is [f ()] with [k] bound to [v]. *)

  (** {1:wake Waiting} *)

  type wake
  (** The type for what fibers wait on. *)

  val wake : unit -> wake
  (** [wake ()] is a new {!type-wake}. *)

  val notify : wake -> unit
  (** [notify w] has the fibers that wait on [w] look again. *)

  val wait : wake -> (unit -> 'a option) -> 'a
  (** [wait w f] is [v] as soon as [f ()] is [Some v]. It calls [f] again after
      each {!notify}, which it must not miss, and is cancelled as its fiber is.
  *)

  (** {1:fibers Fibers} *)

  val in_fiber : unit -> bool
  (** [in_fiber ()] is [true] iff the caller runs in a fiber. *)

  val background : (unit -> unit) list -> (unit -> 'a) -> 'a
  (** [background daemons f] is [f ()], with each of [daemons] running in a
      fiber of its own. Once [f] ends, the daemons still running are cancelled;
      the driver's daemons also return by themselves then. A daemon that raises
      cancels [f], and [background] raises its exception. *)

  val first : (unit -> 'a) -> (unit -> 'a) -> 'a
  (** [first a b] runs [a] and [b] concurrently and is the result of the first
      to end. The other is cancelled. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect f] is [f ()], which a cancellation of the caller does not reach.
  *)

  val check : unit -> unit
  (** [check ()] raises the cancellation of the calling fiber, if any. *)

  val cancelled : exn -> bool
  (** [cancelled e] is [true] iff {!check} or a cancelled fiber raises [e]. *)

  (** {1:io Output} *)

  val write_error : exn -> bool
  (** [write_error e] is [true] iff writing on a broken output raises [e]. *)
end

(** What a backend gets. [Console_eio] documents each function. *)
module type T = sig
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
    (Console.Task.t -> 'a) ->
    'a
  (** [run f] is [f root], with the work under [root] shown on [ppf]. It is
      called in a fiber. *)

  val with_output : (Format.formatter -> 'a) -> 'a
  (** [with_output f] is {!Console.Display.with_output} on the run's display. *)

  val print : Console.View.t -> unit
  (** [print v] is {!Console.Display.print} on the run's display. *)

  val redraw : unit -> unit
  (** [redraw ()] is {!Console.Display.redraw} on the run's display. *)

  val with_terminal : (unit -> 'a) -> 'a
  (** [with_terminal f] is [f ()] with the rows off the screen, and what [f] and
      the fibers it starts write on the standard formatters going straight out,
      as a prompt needs. *)
end

(** One application holds one terminal: a program applies it once. *)
module Make (_ : S) : T

val on_signals :
  ?suspend:((unit -> unit) -> unit) ->
  restore:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** [on_signals ~restore f] is [f ()]. If SIGINT or SIGTERM arrives while [f]
    runs and the program left it at its default disposition, [restore] runs,
    innermost first, then the signal ends the program. If SIGTSTP arrives, the
    process stops inside [suspend stop] (defaults to [stop ()]). A prompt that
    changes the terminal's mode needs it. *)
