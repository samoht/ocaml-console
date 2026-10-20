(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The quantities and caller text a display draws. *)

val bytes : int64 -> string
(** [bytes n] is [n] bytes in the largest binary unit below 1024 of it, a
    negative [n] being [0 B]. *)

val duration : float -> string
(** [duration s] is [s] seconds as a row draws its elapsed time: tenths of a
    second below a minute (["6.4s"]), then minutes and seconds (["1m05s"]), then
    hours and minutes (["1h02m"]). A negative or NaN [s] is ["0.0s"]. *)

val duration_cells : int
(** [duration_cells] is the cells a row's timer is right-aligned in: the widest
    {!duration} below a hundred hours. *)

val clean : string -> string
(** [clean s] is [s] on one line: each control character, line feeds included, a
    space. *)

val lines : string -> string list
(** [lines s] are the lines of [s], each {!clean}: a line ends at a line feed, a
    carriage return or the two together. One trailing line end ends the last
    line rather than starting an empty one; [lines ""] is [[]]. *)
