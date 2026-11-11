(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Row layouts. This is the representation behind {!Progress.Layout}, which
    documents the values both modules share. *)

type kind =
  | Spinner
  | Mark  (** The mark of how the task ended. *)
  | Label of Style.t * int option
      (** A label in this style. With a width, its column is that many cells
          wide. *)
  | Bar of int option
      (** A bar of that many cells. Without a width, the bar grows. *)
  | Percent
  | Count
  | Rate
  | Eta
  | Elapsed
  | Spacer
  | Const of View_core.t
  | Custom of (Task_core.t -> View_core.t)
  | Choose of (Task_core.t -> piece list)

and piece = { kind : kind; stage : int option }
(** The type for the pieces of a row: what each piece shows, and the stage at
    which it gives way, if any. *)

type t
(** The type for layouts. A layout holds at most one {!constructor-Label} and
    one {!constructor-Spacer}. *)

val pieces : t -> piece list
(** [pieces l] are the pieces of [l], left to right. *)

val for_task : t -> Task_core.t -> piece list
(** [for_task l r] are the pieces of [l] on the row of [r], each
    {!constructor-Choose} replaced by the pieces it chooses for [r].

    @raise Invalid_argument if they hold two labels or two spacers. *)

val has_label : piece list -> bool
(** [has_label ps] is [true] iff [ps] holds a {!constructor-Label}. *)

val label_width : t -> int option
(** [label_width l] is the width of the label column, if the label of [l] fixes
    it. *)

val reads : t -> Look.t -> View_core.kind
(** [reads l look] is what a row drawn by [l] in [look] reads: the tasks and the
    time, plus any state that a {!constructor-Custom} piece or a
    {!constructor-Const} view reads. *)

val spinner : t
(** [spinner] is {!Progress.Layout.spinner}. *)

val label : ?style:Style.t -> ?width:int -> unit -> t
(** [label ~style ()] is {!Progress.Layout.label}. *)

val bar : ?width:int -> unit -> t
(** [bar ~width ()] is {!Progress.Layout.bar}. *)

val percent : t
(** [percent] is {!Progress.Layout.percent}. *)

val count : t
(** [count] is {!Progress.Layout.count}. *)

val rate : t
(** [rate] is {!Progress.Layout.rate}. *)

val eta : t
(** [eta] is {!Progress.Layout.eta}. *)

val elapsed : t
(** [elapsed] is {!Progress.Layout.elapsed}. *)

val spacer : t
(** [spacer] is {!Progress.Layout.spacer}. *)

val const : View.t -> t
(** [const v] is {!Progress.Layout.const}. *)

val custom : (Task_core.t -> View.t) -> t
(** [custom f] is {!Progress.Layout.custom}. *)

val choose : (Task_core.t -> t) -> t
(** [choose f] is {!Progress.Layout.choose}. *)

val give_way : int -> t -> t
(** [give_way n l] is {!Progress.Layout.give_way}. *)

val ( ++ ) : t -> t -> t
(** [l0 ++ l1] is [l0] then [l1], as in {!Progress.Layout}. *)

val list : ?sep:t -> t list -> t
(** [list ~sep ls] is {!Progress.Layout.list}. *)

val default : t
(** [default] is {!Progress.Layout.default}. *)

val ended : t
(** [ended] is {!default} with the mark of how the task ended in place of the
    spinner. It draws the line of a task that ended, in the columns of the rows.
*)

val pp : t Fmt.t
(** [pp] formats a layout for diagnostics, in an unspecified format. *)
