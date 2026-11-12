(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks, with the accessors that their views and drivers use. {!Task} is the
    public interface. It documents the values both modules share. *)

(** The type for the progress of a task. *)
type metric = Spin | Count of int * int | Bytes of int64 * int64

type outcome = [ `Succeeded | `Failed | `Cancelled ]
(** The type for task endings. *)

val is_async : exn -> bool
(** [is_async e] is [true] iff [e] is [Sys.Break], [Out_of_memory] or
    [Stack_overflow], possibly wrapped in [Fun.Finally_raised]. Such an
    exception can be raised anywhere, and only the program can handle it. *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~finally f] is [f ()] then [finally ()], even if [f] raises. If
    both raise, the exception of [f] propagates, unless only the exception of
    [finally] is {!is_async}. Neither is wrapped in [Fun.Finally_raised]. *)

val run_all : (unit -> unit) list -> unit
(** [run_all fs] calls each of [fs] in turn, even if one raises. It then raises
    the exception that {!protect} would let propagate. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of Logs messages. *)

(** The type for the kinds of lines. *)
type kind = Output | Logs of level

type line = {
  gap : float;  (** Seconds since the previous line of the task. *)
  kind : kind;
  text : string;  (** One line, without control characters. *)
  seq : int;
      (** The position of the line among the lines of its work. It is [0] for a
          line of the root. *)
}
(** The type for the lines of the output of a task. *)

type memo = ..
(** The type for what a view caches about a task it laid out. *)

type ended = {
  at : float;  (** The time on the clock of the root. *)
  outcome : outcome;
  message : string option;
}
(** The type for how a task ended. *)

type scope = {
  current : unit -> t option;
  within : 'a. t -> (unit -> 'a) -> 'a;
  cancelled : exn -> bool;
}
(** The type for what the tasks know about their scheduler: which task is
    current at the point where [current] is called, how to make a task current,
    and whether an exception means that the fiber that raised it was cancelled.
*)

(** The type for the changes of a work. Its watchers see them in order. A
    {!Line} of the root is a line that belongs to no task. *)
and change =
  | Started of t
  | Progressed of t
  | Renamed of t
  | Line of t * line
  | Ended of t * ended

and work
(** The type for the state shared by the tasks of one root. *)

and t
(** The type for tasks. *)

(** {1:reading Reading} *)

val id : t -> int
(** [id r] is the number of [r] in its work. Only the root has a negative
    number. *)

val work : t -> work
(** [work r] is the work of the root of [r]. *)

val parent : t -> t option
(** [parent r] is {!Task.parent}. *)

val label : t -> string
(** [label r] is {!Task.label}. *)

val metric : t -> metric
(** [metric r] is the progress of [r]. *)

val transient : t -> bool
(** [transient r] is [true] iff [r] is a root or was started transient
    ({!Task.start}). *)

val message : t -> string option
(** [message r] is {!Task.message}. *)

val ended : t -> ended option
(** [ended r] is how [r] ended, or [None] while it runs. *)

val outcome : t -> outcome option
(** [outcome r] is the outcome of {!ended}[ r]. *)

val window : t -> line Queue.t
(** [window r] holds the latest lines of [r], at most [log_lines] of them. It is
    emptied when [r] ends. *)

val output : t -> line Queue.t
(** [output r] holds the lines of [r], kept to show if it fails. The {!dropped}
    oldest lines are not in it. It is emptied when [r] succeeds. *)

val dropped : t -> int
(** [dropped r] is {!Task.dropped}. *)

val gone : t -> bool
(** [gone r] is [true] iff [r] was {!forget}ten. {!children} leaves out such
    tasks. *)

val shed : t -> int
(** [shed r] is the number of tasks under [r] that were forgotten to bound the
    memory held by its subtasks that ended. *)

val rate : t -> float
(** [rate r] is the rate of {!Task.rate}, or [0.] where that is [None]. *)

val version : t -> int
(** [version r] is the number of changes to what [r] shows. *)

val memo : t -> memo list
(** [memo r] is what views cache about [r] ({!keep}). *)

module Work : sig
  val now : work -> unit -> float
  (** [now w ()] is the time on the clock of the root of [w], in seconds. *)

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
  (** [count_cell w] is the width in cells of the count column: the width of the
      widest count allowed by any total seen in [w]. *)

  val short_cell : work -> int
  (** [short_cell w] is the width in cells of the count column once counts get
      shorter: the width of the widest count allowed by any total seen in [w]. A
      count of bytes is then in short units, such as [256M/2.0G]. A count of
      items is unchanged. *)

  val rate_cell : work -> int
  (** [rate_cell w] is the width in cells of the rate column: [0] until a task
      of [w] counts bytes, then at least the widest rate. *)

  val label_cell : work -> int -> int
  (** [label_cell w d] is the width in cells of the label column at {!depth}
      [d]: the widest label that {!widen_label} was given at that depth. *)

  val depths : work -> int
  (** [depths w] is one more than the deepest {!depth} at which {!widen_label}
      was called, or [0] if it never was. *)
end

val widen_label : t -> unit
(** [widen_label r] widens the label column at the depth of [r] to the width of
    the label that [r] draws, if the column is narrower. Once [r] ended, that
    label is its message, if it has one. A view calls [widen_label] for each row
    that draws a label. The column never narrows. *)

val keep : t -> memo list -> unit
(** [keep r m] sets [m] as what views cache about [r]. *)

val on_ending : t -> (t -> unit) -> unit
(** [on_ending root f] sets [f] as the function called on each task of the work
    of [root] when that task starts to end, before its subtasks end. So a driver
    can give the task the rest of its output. *)

val local : unit -> scope
(** [local ()] is a scope without a scheduler. The current task is the task of
    the innermost running {!within}, and no exception is a cancellation. *)

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

val report : ?refused:(exn -> unit) -> t -> level -> string -> unit
(** [report root level text] adds each line of [text], as a message at [level],
    to the output of {!scoped}[ (work root)], or to [root] if that is [None].
    When called from another domain, if adding the message raises [e], the owner
    calls [refused e], unless [e] is {!is_async}. *)

val scoped : work -> t option
(** [scoped w] is the current task of [w] at the point of the call, running or
    not, unless it is the root. *)

val current : work -> t option
(** [current w] is {!scoped}[ w] if that task is running. *)

val settle : t -> unit
(** [settle r] applies the requests that other domains made on the work of [r].
    It does this only in the domain that owns the work, and only when no display
    that watches the work is drawing. It does nothing in another domain. *)

val holding : work -> (unit -> bool) -> unit
(** [holding w f] holds back the requests that other domains made on [w] while
    [f ()] is [true]. *)

val watch : work -> (change -> unit) -> unit
(** [watch w f] calls [f] on each change of [w], after the watchers that were
    added before [f]. *)

val on_event : t -> (Event.t -> unit) -> unit
(** See {!Task.on_event}. *)

val event : change -> time:float -> Event.t option
(** [event c ~time] is the event of [c] at [time]. It is [None] for a
    {!Progressed} task whose metric is {!constructor-Spin}. *)

val forget : t -> unit
(** [forget r] drops [r] and every task under it. They become {!field-gone}. *)

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
(** [depth r] is the number of tasks above [r], the root included. It is [0] for
    the root. *)

val path : t -> string list
(** [path r] is the labels from the top task down to [r], or [[]] for the root.
*)

val root_of : t -> t
(** [root_of r] is the root of the work of [r]. *)

val children : t -> t list
(** [children r] are the tasks under [r] that are not {!field-gone}, in the
    order they started. *)

val running_children : t -> t list
(** [running_children r] are the running tasks directly under [r], the latest
    first. *)

val count_text : metric -> string
(** [count_text m] is what the count of a row shows: the items or bytes done
    over the total, or [""] for {!constructor-Spin}. *)

val rate_text : metric -> float -> string
(** [rate_text m rate] is what the rate of a row shows: [rate] bytes per second
    for {!constructor-Bytes} once [rate] is above one, or [""] otherwise. *)

val fraction : metric -> float option
(** [fraction m] is the fraction of [m] done, or [None] for {!constructor-Spin}.
*)

val eta : t -> float option
(** [eta r] is the number of seconds [r] has left at its rate, if it runs and
    has a rate. *)

val pp : t Fmt.t
(** [pp] is {!Task.pp}. *)
