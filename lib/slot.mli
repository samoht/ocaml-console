(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Values kept on a formatter.

    This module is internal to the [console] package: [Console] does not expose
    it, and [console.tty] reads it through [Console__].

    A slot holds one value per formatter, kept as {!Fmt} keeps its style
    renderer: in the semantic tag functions of the formatter, so that it lives
    as long as the formatter and {!Fmt.with_buffer}[ ~like] carries it over. *)

type 'a t
(** The type for slots holding values of type ['a]. *)

val v : ?setting:bool -> unit -> 'a t
(** [v ()] is a new slot, empty on every formatter. A value it holds is a
    setting of the formatter iff [setting] (defaults to [true]), as {!unset}
    sees it. *)

val find : 'a t -> Format.formatter -> 'a option
(** [find s ppf] is the value of [s] on [ppf], if {!set} gave it one. *)

val set : 'a t -> Format.formatter -> 'a -> unit
(** [set s ppf v] makes [v] the value of [s] on [ppf]. *)

val generation : unit -> int
(** [generation ()] is the number of {!set}s so far, on any slot and formatter,
    so that a reader of a slot knows when to read it again. *)

val unset : Format.formatter -> bool
(** [unset ppf] is [true] iff nothing but slots that are not settings changed
    the tag functions of [ppf]: neither {!Fmt}, such as its style renderer, nor
    a slot that is a setting. *)
