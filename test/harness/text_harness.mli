(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bytes of text for the tests of [Console_text], and of what prints text. *)

val is_ascii : string -> bool
(** [is_ascii s] is [true] iff every byte of [s] is below 0x80. *)

val maximal_subparts : (string * string * string) list
(** [maximal_subparts] are the vectors of Unicode Core Spec tables 3-8 to 3-11
    (section 3.9, "U+FFFD Substitution of Maximal Subparts"): each table's name,
    its bytes and what they convert to. *)

val silent : int list
(** [silent] is the controls that a terminal acts on without printing, including
    CAN and SUB, plus DEL. *)
