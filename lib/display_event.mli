(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The events of a display whose form is {!Display_state.Json}, each a
    {!Progress.Event.t} written as one JSON object on a line of its own. *)

open Display_state

val is_json : t -> bool
(** [is_json t] is [true] iff [t] writes events. *)

val started : row -> unit
(** [started r] writes that [r] started. *)

val progress : row -> unit
(** [progress r] writes the count of [r]. *)

val renamed : row -> unit
(** [renamed r] writes the label of [r], which changed. *)

val output : t -> row option -> line -> unit
(** [output t r l] writes [l], a line of output of [r], or of no task. *)

val ended : row -> unit
(** [ended r] writes how [r] ended. *)

val result : t -> unit
(** [result t] writes the result of the work, if [t] has one. *)
