(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running tasks for a scheduler.

    This module is internal to console. [Console] does not expose it.
    [console.driver] reads it through [Console__].

    A driver runs the tasks of a root for a scheduler, as [Console_driver] does.
    Its {!type-scope} says which task is current in a fiber. The driver passes
    the [Logs] messages of the program to {!report}. It gives what the program
    prints to the task that {!current} returns. *)

type scope = Task_core.scope = {
  current : unit -> Task.t option;
      (** [current ()] is the current task at the point of the call. *)
  within : 'a. Task.t -> (unit -> 'a) -> 'a;
      (** [within task f] is [f ()] with [task] current in [f] and in the fibers
          that [f] starts. *)
  cancelled : exn -> bool;
      (** [cancelled e] is [true] iff [e] means that the fiber that raises it
          was cancelled. If the function of a {!Task.run} raises such an [e],
          the task is cancelled instead of failed. *)
}
(** The type for what the tasks know about their scheduler. *)

val root : ?label:string -> scope:scope -> now:(unit -> float) -> unit -> Task.t
(** [root ~scope ~now ()] is {!Task.root}[ ~now ()], where [scope] says which
    task is current. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of messages, as in [Logs]. *)

val report : ?refused:(exn -> unit) -> Task.t -> level -> string -> unit
(** [report root level text] adds each line of [text], as a message at [level],
    to the output of the current task under [root]. If no task is current, the
    lines go to [root]. When called from another domain, [report] returns at
    once. Then, if adding the message raises [e], the domain that owns [root]
    calls [refused e] (defaults to doing nothing), unless [e] is {!is_async}. *)

val current : Task.t -> Task.t option
(** [current root] is the current task under [root] at the point of the call, if
    it is running. A driver gives it what the program prints, with {!Task.log}.
*)

val ending : Task.t -> (Task.t -> unit) -> unit
(** [ending root f] calls [f] on [root] and on each task under it when that task
    starts to end, before its subtasks end. So a driver can give the task the
    rest of its output. *)

val id : Task.t -> int
(** [id task] is the number of [task] among the tasks of its root. Only the root
    has a negative number. *)

val is_async : exn -> bool
(** [is_async e] is [true] iff [e] is an interrupt, or an out-of-memory or stack
    overflow error. Only the program can handle these. *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~finally f] is [f ()] then [finally ()], even if [f] raises. If
    both raise, the exception of [f] propagates, unless only the exception of
    [finally] is {!is_async}. *)
