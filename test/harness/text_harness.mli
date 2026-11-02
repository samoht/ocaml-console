(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bytes of text for the tests of [Console_text]. *)

val silent : int list
(** [silent] are the controls a terminal acts on without printing, CAN and SUB
    among them, and DEL. *)
