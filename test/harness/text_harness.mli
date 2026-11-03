(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bytes of text for the tests of [Console_text], and of what prints text. *)

val is_ascii : string -> bool
(** [is_ascii s] is [true] iff every byte of [s] is below 0x80. *)

val lines : string -> string list
(** [lines s] are the lines of [s] between its line feeds: one more than [s] has
    line feeds. *)

val silent : int list
(** [silent] are the controls a terminal acts on without printing, CAN and SUB
    among them, and DEL. *)
