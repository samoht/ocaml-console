(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running a display for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__].

    A driver runs a display as [Console_eio.run] does for Eio: it says with a
    {!type-scope} which task is current in a fiber, passes the program's [Logs]
    messages to {!report}, and gives the current task what the program prints on
    the display's terminal ({!current}). *)

type scope
(** The type for the ways to tell which task is current. *)

val scope :
  current:(unit -> Display.task option) ->
  within:(Display.task -> (unit -> unit) -> unit) ->
  cancelled:(exn -> bool) ->
  scope
(** [scope ~current ~within ~cancelled] is the scope in which [current ()] is
    the task current where it is called, [within task f] calls [f] with [task]
    current there and in what [f] starts, and [cancelled e] is [true] iff [e]
    says that the fiber raising it was cancelled: a {!Display.with_task} whose
    function raises it cancels its task rather than failing it. *)

type form = [ `Plain | `Quiet | `Json ]
(** The type for what a display prints without a terminal, as the [`Plain],
    [`Quiet] and [`Json] forms of {!Progress.t} describe. *)

val v :
  ?layout:Display.Layout.t ->
  ?header:string ->
  ?size:(unit -> int * int) ->
  ?form:form ->
  scope:scope ->
  now:(unit -> float) ->
  Format.formatter ->
  Display.t
(** [v ~form ~scope ~now ppf] is {!Display.v}[ ~now ppf] in which [scope] says
    which task is current and, without [size], [form] says what the display
    prints (defaults to [`Plain]). *)

val is_json : Display.t -> bool
(** [is_json display] is [true] iff [display] prints JSON events, so that what
    the program prints on its formatter goes as events too ({!Display.log}). *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of messages, those of [Logs]. *)

val report : Display.t -> level -> string -> unit
(** [report display level text] gives each line of [text], a message of [level],
    to the current task as its output ({!Display.output}), or prints it above
    the region when there is none. A message of level [`Warning], [`Error] or
    [`Debug] comes after the mark of its level, one of level [`App] or [`Info]
    after none. *)

val current : Display.t -> Display.task option
(** [current display] is the task of [display] current where it is called, if it
    still runs and is not the {!Display.root}. A driver gives it what the
    program prints on the display's terminal meanwhile, with {!Display.log}. *)
