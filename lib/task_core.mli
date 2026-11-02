(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks, with what their views and drivers read. {!Task} is their public face
    and documents the values both have. *)

(** The type for how far a task is. *)
type metric = Spin | Count of int * int | Bytes of int64 * int64

type outcome = [ `Succeeded | `Failed | `Cancelled ]
(** The type for task endings. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of Logs messages. *)

(** The type for the kinds of lines. *)
type kind = Output | Logs of level

type line = {
  gap : float;  (** Seconds since the task's previous line. *)
  kind : kind;
  text : string;  (** One line, without control characters. *)
  seq : int;  (** The order of the line among those of its work. *)
}
(** The type for the lines of a task's output. *)

type memo = ..
(** The type for what a view keeps of a task it laid out. *)

type scope = {
  current : unit -> t option;
  within : t -> (unit -> unit) -> unit;
  cancelled : exn -> bool;
}
(** The type for what the tasks know of their scheduler: which task is current
    where it is asked, how to make one current, and whether an exception says
    that the fiber raising it was cancelled. *)

(** The type for the changes of a work, which its watchers see in order. A
    {!Line} of the root is output of no task. *)
and change =
  | Started of t
  | Progressed of t
  | Renamed of t
  | Line of t * line
  | Ended of t

and work
(** The type for what the tasks of one root share. *)

and t
(** The type for tasks. *)

(** {1:reading Reading} *)

val id : t -> int
(** [id r] is [r]'s number in its work, negative for the root alone. *)

val work : t -> work
(** [work r] is what [r] shares with the other tasks of its root. *)

val parent : t -> t option
(** [parent r] is {!Task.parent}. *)

val label : t -> string
(** [label r] is {!Task.label}. *)

val metric : t -> metric
(** [metric r] is how far [r] is. *)

val transient : t -> bool
(** [transient r] is [true] iff [r] is a root or was started transient
    ({!Task.start}). *)

val message : t -> string option
(** [message r] is {!Task.message}. *)

val outcome : t -> outcome option
(** [outcome r] is how [r] ended, [None] while it runs. *)

val window : t -> line Queue.t
(** [window r] holds [r]'s latest lines, as many as it shows. *)

val output : t -> line Queue.t
(** [output r] holds [r]'s lines, kept for its failure, its {!dropped} oldest
    left out. *)

val dropped : t -> int
(** [dropped r] is {!Task.dropped}. *)

val gone : t -> bool
(** [gone r] is [true] once [r] is forgotten, and its parent then forgets it. *)

val shed : t -> int
(** [shed r] counts the tasks under [r] dropped to keep those that ended and
    wait bounded. *)

val rate : t -> float
(** [rate r] is the rate of {!Task.rate}, [0.] where that is [None]. *)

val version : t -> int
(** [version r] counts the changes to what [r] shows. *)

val memo : t -> memo list
(** [memo r] is what views keep of [r] ({!keep}). *)

module Work : sig
  val now : work -> unit -> float
  (** [now w ()] is the time on the clock of [w]'s root, in seconds. *)

  val summary : work -> string option
  (** [summary w] is the label of the root, if it was given one. *)

  val started : work -> int
  (** [started w] is the number of tasks started in [w], without the root. *)

  val finished : work -> int
  (** [finished w] is the number of tasks of [w] that ended, without the root.
  *)

  val failed : work -> int
  (** [failed w] is the number of tasks of [w] that failed, without the root. *)

  val cancelled : work -> int
  (** [cancelled w] is the number of tasks of [w] that were cancelled, without
      the root. *)

  val count_cell : work -> int
  (** [count_cell w] is the cells of the widest count a total has allowed. *)

  val rate_cell : work -> int
  (** [rate_cell w] is the cells of the widest rate seen. *)

  val label_cell : work -> int -> int
  (** [label_cell w d] is the cells of the widest label {!widen_label} gave at
      {!depth} [d]. *)

  val depths : work -> int
  (** [depths w] is one more than the deepest {!depth} of [w]'s tasks. *)
end

val widen_label : t -> unit
(** [widen_label r] widens the label column of [r]'s depth to [r]'s label, if it
    is narrower: a view does it for each row that draws a label. *)

val keep : t -> memo list -> unit
(** [keep r m] makes [m] what views keep of [r]. *)

val on_ending : t -> (t -> unit) -> unit
(** [on_ending root f] has [f] called with each task of [root]'s work as it
    ends, before it ends, so that a driver gives it the last of its output. *)

val local : unit -> scope
(** [local ()] is the scope in which the current task is that of the innermost
    {!within} that runs. *)

val root : ?label:string -> ?scope:scope -> now:(unit -> float) -> unit -> t
(** See {!Task.root}. [scope] defaults to {!local}[ ()]. *)

val start :
  ?key:string -> ?log_lines:int -> ?transient:bool -> t -> string -> t * bool
(** [start parent label] is {!Task.start}, and whether the task is new. *)

val run :
  ?key:string ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  (t -> 'a) ->
  'a
(** See {!Task.run}. *)

val within : t -> (unit -> 'a) -> 'a
(** See {!Task.within}. *)

val set_label : t -> string -> unit
(** [set_label r label] is {!Task.set_label}. *)

val set_items : t -> count:int -> total:int -> unit
(** [set_items r ~count ~total] is {!Task.set_items}. *)

val set_bytes : t -> count:int64 -> total:int64 -> unit
(** [set_bytes r ~count ~total] is {!Task.set_bytes}. *)

val advance : ?by:int -> t -> unit
(** [advance ~by r] is {!Task.advance}. *)

val log : ?level:level -> t -> string -> unit
(** [log ~level r text] is {!Task.log}. *)

val succeed : ?message:string -> t -> unit
(** [succeed ~message r] is {!Task.succeed}. *)

val fail : ?message:string -> t -> unit
(** [fail ~message r] is {!Task.fail}. *)

val cancel : ?message:string -> t -> unit
(** [cancel ~message r] is {!Task.cancel}. *)

val report : t -> level -> string -> unit
(** [report root level text] gives each line of [text], a message of [level], to
    the task of [root]'s work current where it is called, or to [root] when
    there is none. *)

val scoped : work -> t option
(** [scoped w] is the task of [w] current where it is called, if it is not the
    root, running or not. *)

val current : work -> t option
(** [current w] is {!scoped}[ w] if it runs. *)

val watch : work -> (change -> unit) -> unit
(** [watch w f] calls [f] with each change of [w], after the watchers before it.
*)

val on_event : t -> (Event.t -> unit) -> unit
(** See {!Task.on_event}. *)

val event : change -> time:float -> Event.t option
(** [event c ~time] is the event of [c] at [time], if it has one: a count never
    goes back to a spinner. *)

val forget : t -> unit
(** [forget r] drops [r] and every task under it: they are {!field-gone}. *)

(** {1:read Reading tasks} *)

val is_root : t -> bool
(** [is_root r] is [true] iff [r] is the root of its work. *)

val is_running : t -> bool
(** [is_running r] is [true] iff [r] has not ended. *)

val state : t -> [ `Running | outcome ]
(** [state r] is {!Task.state}. *)

val elapsed : t -> float
(** [elapsed r] is {!Task.elapsed}. *)

val since_start : work -> float
(** [since_start w] is the seconds since the root of [w] started. *)

val depth : t -> int
(** [depth r] is the number of tasks above [r], the root included: [0] for the
    root. *)

val path : t -> string list
(** [path r] is the labels from the top task down to [r], [[]] for the root. *)

val root_of : t -> t
(** [root_of r] is the root of [r]'s work. *)

val children : t -> t list
(** [children r] are the tasks under [r] that are not {!field-gone}, in the
    order they started. *)

val running_children : t -> t list
(** [running_children r] are the running tasks directly under [r], the latest
    first. *)

val count_text : metric -> string
(** [count_text m] is what a row's count shows: items or bytes done over the
    total, or [""] for {!constructor-Spin}. *)

val rate_text : metric -> float -> string
(** [rate_text m rate] is what a row's rate shows: [rate] bytes per second for
    {!constructor-Bytes} once it is above one, or [""]. *)

val fraction : metric -> float option
(** [fraction m] is the share of the work done, if [m] says. *)

val eta : t -> float option
(** [eta r] is the seconds [r] has left at its rate, if it runs and has one. *)

val pp : t Fmt.t
(** [pp] is {!Task.pp}. *)
