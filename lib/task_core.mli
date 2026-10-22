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

and work = private {
  now : unit -> float;
  start : float;
  summary : string option;  (** The label of the root, if it was given one. *)
  scope : scope;
  by_key : (string, t) Hashtbl.t;
  mutable next_id : int;
  mutable next_line : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable count_cell : int;
      (** The cells of the widest count a total has allowed. *)
  mutable rate_cell : int;  (** The cells of the widest rate seen. *)
  mutable watchers : (change -> unit) list;
  mutable ending : t -> unit;  (** Called with each task as it ends, first. *)
  mutable changes : int;  (** The number of changes so far. *)
}
(** The type for what the tasks of one root share. *)

and t = private {
  id : int;  (** Negative for the root alone. *)
  key : string option;
  work : work;
  parent : t option;
  log_lines : int;
  window : line Queue.t;
  output : line Queue.t;
  mutable output_bytes : int;
  mutable dropped : int;
  mutable log_prev : float;
  mutable label : string;
  mutable metric : metric;
  transient : bool;
  started_at : float;
  mutable ended_at : float option;
  mutable message : string option;
  mutable outcome : outcome option;
  mutable children : t list;  (** The latest first. *)
  mutable child_limit : int;
  mutable gone : bool;
  mutable held : int;
  mutable weight : int;
  mutable shed : int;
  mutable last_cur : float;
  mutable last_t : float;
  mutable rate : float;
  mutable version : int;  (** Counts the changes to what the task shows. *)
  mutable memo : memo list;
}
(** The type for tasks. A task's {!field-window} holds its latest lines, at most
    {!field-log_lines}, and its {!field-output} its lines, kept for its failure,
    {!field-output_bytes} bytes in all, the {!field-dropped} oldest left out. A
    task is {!field-gone} once forgotten, and its parent then forgets it. A task
    that ended and waits weighs {!field-weight} bytes, and the ones under a task
    {!field-held} bytes in all; {!field-shed} counts the tasks under it dropped
    to keep that bounded. *)

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
val set_items : t -> count:int -> total:int -> unit
val set_bytes : t -> count:int64 -> total:int64 -> unit
val log : ?level:level -> t -> string -> unit
val succeed : ?message:string -> t -> unit
val fail : ?message:string -> t -> unit
val cancel : ?message:string -> t -> unit

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
val is_running : t -> bool
val state : t -> [ `Running | outcome ]
val elapsed : t -> float
val since_start : work -> float
val depth : t -> int

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
