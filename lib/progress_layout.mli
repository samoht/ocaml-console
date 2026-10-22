(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Row layouts: the representation behind {!Progress.Layout}, which documents
    the values both have. *)

type kind =
  | Spinner
  | Label
  | Bar of int option  (** A bar that many cells wide, or that grows. *)
  | Percent
  | Count
  | Rate
  | Eta
  | Elapsed
  | Spacer
  | Const of View_core.t
  | Custom of (Task_core.t -> View_core.t)

type piece = { kind : kind; stage : int option }
(** The type for the pieces of a row: what each shows, and the stage at which it
    gives way, if it does. *)

type t
(** The type for layouts: at most one {!constructor-Label} and one
    {!constructor-Spacer}. *)

val pieces : t -> piece list
(** [pieces l] are the pieces of [l], left to right. *)

val spinner : t
val label : t
val bar : ?width:int -> unit -> t
val percent : t
val count : t
val rate : t
val eta : t
val elapsed : t
val spacer : t
val const : View.t -> t
val custom : (Task_core.t -> View.t) -> t
val give_way : int -> t -> t
val ( ++ ) : t -> t -> t
val list : ?sep:t -> t list -> t
val default : t

val pp : t Fmt.t
(** [pp] formats a layout for diagnostics, in an unspecified format. *)
