(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running tasks for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__].

    A driver runs the tasks of a root for a scheduler, as [Console_eio.run] does
    for Eio. Its {!type-scope} says which task is current in a fiber. It passes
    the program's [Logs] messages to {!report}, and gives what the program
    prints to the task that {!current} returns. *)

type scope = Task_core.scope
(** The type for what the tasks know of their scheduler. *)

val scope :
  current:(unit -> Task.t option) ->
  within:(Task.t -> (unit -> unit) -> unit) ->
  cancelled:(exn -> bool) ->
  scope
(** [scope ~current ~within ~cancelled] is the scope in which:
    - [current ()] is the task current where [current] is called;
    - [within task f] calls [f] with [task] current in [f] and in the fibers
      that [f] starts;
    - [cancelled e] is [true] iff [e] means that the fiber raising it was
      cancelled. A {!Task.run} whose function raises such an [e] cancels its
      task instead of failing it. *)

val root : ?label:string -> scope:scope -> now:(unit -> float) -> unit -> Task.t
(** [root ~scope ~now ()] is {!Task.root}[ ~now ()] in which [scope] says which
    task is current. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of messages, those of [Logs]. *)

val report : Task.t -> level -> string -> unit
(** [report root level text] adds each line of [text], a message at [level], to
    the output of the task under [root] that is current where [report] is
    called, or to [root] if none is. *)

val current : Task.t -> Task.t option
(** [current root] is the task under [root] that is current where [current] is
    called, if it is running. A driver gives it what the program prints, with
    {!Task.log}. *)

val ending : Task.t -> (Task.t -> unit) -> unit
(** [ending root f] has [f] called with [root] and with each task under it when
    that task starts to end, before the tasks under it end, so that a driver can
    give it the last of its output. *)

val id : Task.t -> int
(** [id task] is the number of [task] among the tasks of its root. Only the root
    has a negative number. *)

val is_async : exn -> bool
(** [is_async e] is [true] iff [e] is an interrupt or an exhausted heap or
    stack, which only the program can handle. *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~finally f] is [f ()] then [finally ()], whatever [f] raises. If
    both raise, the exception of [f] propagates, unless only that of [finally]
    is {!is_async}. *)
