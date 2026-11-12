(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Values kept on a formatter.

    This module is internal to the [console] package. [Console] does not expose
    it. [console.tty] uses it through [Console__].

    A slot holds one value per formatter. Like {!Fmt}'s style renderer, the
    value is stored in the formatter's semantic tag functions. So it lives as
    long as the formatter, and {!Fmt.with_buffer}[ ~like] copies it. *)

type 'a t
(** The type for slots holding values of type ['a]. *)

val v : ?setting:bool -> ?watched:bool -> unit -> 'a t
(** [v ()] is a new slot, empty on every formatter. If [setting] is [true] (the
    default), a value in the slot counts as a setting of the formatter for
    {!unset}. If [watched] is [true] (defaults to [false]), each {!set}
    increments {!generation}. *)

val find : 'a t -> Format.formatter -> 'a option
(** [find s ppf] is the value of [s] on [ppf], if {!set} gave it one. *)

val set : 'a t -> Format.formatter -> 'a -> unit
(** [set s ppf v] makes [v] the value of [s] on [ppf]. *)

val generation : unit -> int
(** [generation ()] is the number of {!set} calls so far on watched slots
    ([~watched:true]), on any formatter. A reader of a watched slot uses it to
    know when to read the slot again. *)

val changed : unit -> unit
(** [changed ()] increments {!generation}, as {!set} on a watched slot does.
    Call it when the value used for an empty watched slot changes, so that
    readers read the slot again. *)

val unset : Format.formatter -> bool
(** [unset ppf] is [true] iff nothing but non-setting slots changed the tag
    functions of [ppf]. That is, neither {!Fmt} (for example to set its style
    renderer) nor a setting slot changed them. *)
