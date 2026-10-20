(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Values kept on a formatter.

    A slot holds one value per formatter, kept as {!Fmt} keeps its style
    renderer: in the semantic tag functions of the formatter, so that it lives
    as long as the formatter and {!Fmt.with_buffer}[ ~like] carries it over. *)

type 'a t
(** The type for slots holding values of type ['a]. *)

val v : unit -> 'a t
(** [v ()] is a new slot, empty on every formatter. *)

val find : 'a t -> Format.formatter -> 'a option
(** [find s ppf] is the value of [s] on [ppf], if {!set} gave it one. *)

val set : 'a t -> Format.formatter -> 'a -> unit
(** [set s ppf v] makes [v] the value of [s] on [ppf]. *)
