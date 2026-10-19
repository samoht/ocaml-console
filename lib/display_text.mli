(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The quantities and caller text a display draws. *)

val bytes : int64 -> string
(** [bytes n] is [n] bytes in the largest suitable binary unit. *)

val span : float -> string
(** [span seconds] is [seconds] as a row draws its elapsed time: tenths of a
    second below a minute (["6.4s"]), then minutes and seconds (["1m05s"]), then
    hours and minutes (["1h02m"]). A negative [seconds] is ["0.0s"]. *)

val elapsed_cell : int
(** [elapsed_cell] is the number of columns a row's timer is right-aligned in:
    the widest {!span} below a hundred hours, so a running timer moves nothing
    to its left. *)

val sanitize : string -> string
(** [sanitize s] is [s] as one line of valid UTF-8: a malformed sequence is a
    replacement character and a control character, newlines included, is a
    space. *)

val lines : string -> string list
(** [lines s] are the lines of [s], with a trailing ['\r'] removed from each.
    One trailing newline ends the last line rather than starting an empty one;
    [lines ""] is [[]]. *)

val sanitize_multiline : string -> string
(** [sanitize_multiline s] is [s] with each of its {!lines} {!sanitize}d. *)
