(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Displays, with what a driver needs. {!Display} is their public face and
    documents the values both have; {!Display_driver} gives a driver the rest.
*)

module Layout = Display_layout

type t = Display_state.t
(** The type for displays. *)

type task = Display_state.row
(** The type for tasks. *)

type scope = Display_state.scope
(** See {!Display_driver.type-scope}. *)

type level = Display_state.level
(** See {!Display_driver.type-level}. *)

val scope :
  current:(unit -> task option) ->
  within:(task -> (unit -> unit) -> unit) ->
  cancelled:(exn -> bool) ->
  write_error:(exn -> bool) ->
  scope
(** See {!Display_driver.scope}. *)

val v :
  ?layout:Layout.t ->
  ?summary:string ->
  ?view:(t -> width:int -> height:int -> Span.t) ->
  ?keep:bool ->
  ?size:(unit -> int * int) ->
  ?progress:Progress.t ->
  ?scope:scope ->
  now:(unit -> float) ->
  Format.formatter ->
  t
(** [v ~scope ~now ppf] is {!Display.v}[ ~now ppf] in which [scope] says which
    task is current (defaults to the task of the innermost {!with_task} that
    runs). *)

val redraw : t -> unit
(** See {!Display.redraw}. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** See {!Display.with_output}. *)

val set_view : t -> (t -> width:int -> height:int -> Span.t) -> unit
(** See {!Display.set_view}. *)

val default_view : t -> width:int -> height:int -> Span.t
(** See {!Display.default_view}. *)

val tasks : t -> task list
(** See {!Display.tasks}. *)

val tree : ?width:int -> ?height:int -> task -> Span.t
(** See {!Display.tree}. *)

val summary : t -> Span.t
(** See {!Display.summary}. *)

val set_result : ?detail:string -> t -> (string, string) result -> unit
(** See {!Display.set_result}. *)

val close : t -> unit
(** See {!Display.close}. *)

val pp : t Fmt.t
(** See {!Display.pp}. *)

val task :
  ?key:string ->
  ?parent:task ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  task
(** See {!Display.val-task}. *)

val with_task :
  ?key:string ->
  ?parent:task ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  (task -> 'a) ->
  'a
(** See {!Display.with_task}. *)

val within : task -> (unit -> 'a) -> 'a
(** See {!Display.within}. *)

val root : t -> task
(** See {!Display.root}. *)

val iter : t -> string -> ('a -> unit) -> 'a list -> unit
(** See {!Display.iter}. *)

val set_label : task -> string -> unit
(** See {!Display.set_label}. *)

val set_count : task -> cur:int -> total:int -> unit
(** See {!Display.set_count}. *)

val set_bytes : task -> cur:int64 -> total:int64 -> unit
(** See {!Display.set_bytes}. *)

val log : task -> string -> unit
(** See {!Display.log}. *)

val succeed : ?message:string -> task -> unit
(** See {!Display.succeed}. *)

val fail : ?message:string -> task -> unit
(** See {!Display.fail}. *)

val cancel : ?message:string -> task -> unit
(** See {!Display.cancel}. *)

val report : t -> level -> string -> unit
(** See {!Display_driver.report}. *)

val current : t -> task option
(** See {!Display_driver.current}. *)

val watch :
  t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** See {!Display_driver.watch}. *)
