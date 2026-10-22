(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Temporary workarounds for matrix 0.1.0's emulator, each fixed upstream.

    Deleted when console moves to matrix 0.2.0. *)

val scan : Ansi.Parser.t -> string -> int -> (Ansi.Parser.token -> unit) -> unit
(** [scan p s i f] feeds byte [i] of [s] to [p], holding a multi-byte character
    back until its last byte. *)

val backspace : string -> string
(** [backspace s] is [s] with each BS written as CUB 1. *)
