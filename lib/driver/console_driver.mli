(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running console on a scheduler.

    A backend passes to {!Make} what console needs from a scheduler. It gets
    back {!T.run}, {!T.print}, {!T.with_output} and {!T.redraw}, all in direct
    style. [run] does the rest: it finds the terminal, shows the work, captures
    the program's output and its [Logs] messages, redraws, and gives the
    terminal back.

    This library is [console.driver]. [console-eio] and [console-lwt] build on
    it. *)

(** What a scheduler gives console. A {e fiber} is anything that the scheduler
    runs concurrently in a domain. The driver calls {!S.notify} from any thread,
    domain or signal handler. It calls every other function only on the first
    thread of a domain, in a domain that starts a [run] or makes a task current.
    A scheduler without cancellation implements [first a _] as [a ()],
    [protect f] as [f ()], and [check ()] as doing nothing. *)
module type S = sig
  (** {1:clock Time} *)

  val clock : unit -> unit -> float
  (** [clock () ()] is the time that the display shows and records, in seconds
      from any origin. The driver calls [clock ()] when a display opens, in a
      fiber of its run. It may call the returned function from any domain. *)

  val tick : float -> unit
  (** [tick d] waits [d] seconds of {!val-clock} time between two redraws. *)

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
  (** [notify w] makes the fibers that wait on [w] check their condition again.
  *)

  val wait : wake -> (unit -> 'a option) -> 'a
  (** [wait w f] is [v] as soon as [f ()] is [Some v]. It calls [f] again after
      each {!notify} on [w], and must not miss one. It is cancelled when its
      fiber is cancelled. *)

  (** {1:fibers Fibers} *)

  val in_fiber : unit -> bool
  (** [in_fiber ()] is [true] iff the caller runs in a fiber. *)

  val background : (unit -> unit) list -> (unit -> 'a) -> 'a
  (** [background daemons f] is [f ()], with each of [daemons] running in its
      own fiber. When [f] ends, the daemons still running are cancelled. The
      driver's own daemons also return by themselves at that point. If a daemon
      raises, [f] is cancelled and [background] raises that exception. *)

  val first : (unit -> 'a) -> (unit -> 'a) -> 'a
  (** [first a b] runs [a] and [b] concurrently and is the result of the first
      to end. The other is cancelled. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect f] is [f ()]. Cancelling the caller does not cancel [f]. *)

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
  (** [run f] is [f root], with the work under [root] shown on [ppf]. Call it in
      a fiber. *)

  val with_output : (Format.formatter -> 'a) -> 'a
  (** [with_output f] is {!Console.Display.with_output} on the run's display. *)

  val print : Console.View.t -> unit
  (** [print v] is {!Console.Display.print} on the run's display. *)

  val redraw : unit -> unit
  (** [redraw ()] is {!Console.Display.redraw} on the run's display. *)

  val with_terminal : (unit -> 'a) -> 'a
  (** [with_terminal f] is [f ()] with the progress rows cleared from the
      screen. What [f] and its fibers write on the standard formatters goes
      straight out. A prompt needs this. *)
end

(** A program applies [Make] only once, because a program has one terminal. *)
module Make (_ : S) : T

val on_signals :
  ?suspend:((unit -> unit) -> unit) ->
  restore:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** [on_signals ~restore f] is [f ()]. If SIGINT or SIGTERM arrives while [f]
    runs and the program left it at its default disposition, [restore] runs
    (innermost call first), then the signal ends the program. If SIGTSTP
    arrives, the process stops inside [suspend stop] (defaults to [stop ()]).
    Use it in a prompt that changes the terminal's mode. *)
