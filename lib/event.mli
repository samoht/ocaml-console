(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The events of a task, as {!Task.Event} documents them. *)

type kind = [ `Start | `Progress | `Label | `Output | `End | `Other of string ]
(** See {!Task.Event.type-kind}. *)

type count_unit = [ `Items | `Bytes | `Other of string ]
(** See {!Task.Event.type-count_unit}. *)

type level = [ `App | `Error | `Warning | `Info | `Debug | `Other of string ]
(** See {!Task.Event.type-level}. *)

type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
(** See {!Task.Event.type-status}. *)

type task = int * string list
(** See {!Task.Event.type-task}. *)

type t
(** The type for events. *)

val start : ?parent:int -> time:float -> task -> t
(** [start ~parent ~time task] is {!Task.Event.val-start}. *)

val progress :
  time:float -> count:int64 -> total:int64 -> unit:count_unit -> task -> t
(** [progress ~time ~count ~total ~unit task] is {!Task.Event.val-progress}. *)

val label : time:float -> task -> string -> t
(** [label ~time task text] is {!Task.Event.val-label}. *)

val output : ?task:task -> ?level:level -> time:float -> string -> t
(** [output ~task ~level ~time text] is {!Task.Event.val-output}. *)

val succeeded :
  ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
(** [succeeded ~task ~message ~time ~elapsed ()] is {!Task.Event.val-succeeded}.
*)

val failed :
  ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
(** [failed] is {!Task.Event.val-failed}. *)

val cancelled :
  ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
(** [cancelled] is {!Task.Event.val-cancelled}. *)

val other : ?task:task -> time:float -> string -> t
(** [other ~task ~time name] is {!Task.Event.val-other}. *)

val time : t -> float
(** [time e] is {!Task.Event.val-time}. *)

val kind : t -> kind
(** [kind e] is {!Task.Event.val-kind}. *)

val task : t -> task option
(** [task e] is {!Task.Event.val-task}. *)

val parent : t -> int option
(** [parent e] is {!Task.Event.val-parent}. *)

val count : t -> int64 option
(** [count e] is {!Task.Event.val-count}. *)

val total : t -> int64 option
(** [total e] is {!Task.Event.val-total}. *)

val unit : t -> count_unit option
(** [unit e] is {!Task.Event.val-unit}. *)

val status : t -> status option
(** [status e] is {!Task.Event.val-status}. *)

val elapsed : t -> float option
(** [elapsed e] is {!Task.Event.val-elapsed}. *)

val text : t -> string option
(** [text e] is {!Task.Event.val-text}. *)

val message : t -> string option
(** [message e] is {!Task.Event.val-message}. *)

val level : t -> level option
(** [level e] is {!Task.Event.val-level}. *)

val replaced : (Uchar.t -> bool) -> string -> string
(** [replaced keep s] is [s] as valid UTF-8 of the characters that [keep]: each
    ill-formed sequence, and each character [keep] refuses, is one U+FFFD. *)

val jsont : t Jsont.t
(** [jsont] is {!Task.Event.val-jsont}. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is {!Task.Event.val-of_string}. *)

val to_string : t -> string
(** [to_string e] is {!Task.Event.val-to_string}. *)

val pp : Format.formatter -> t -> unit
(** [pp] is {!Task.Event.val-pp}. *)

val equal : t -> t -> bool
(** [equal e0 e1] is {!Task.Event.val-equal}. *)
