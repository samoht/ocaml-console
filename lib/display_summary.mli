(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The session's header, verdict and detail lines. *)

open Display_state

val header : t -> fit:bool -> string option
(** [header t ~fit] is the header line of [t], if [t] has a header: a marker for
    the state of the session, its name, its elapsed time and its finished/total
    count. With [fit], the line is at most [t.width] columns. *)

val verdict : t -> [ `Ok of string | `Fail of string ] -> string
(** [verdict t r] is the verdict line of [r]. On a terminal a long message wraps
    under a hanging indent, as it is written after the region is released. *)

val detail : t -> string option -> string list
(** [detail t d] are the dimmed lines of the verdict's detail [d], wrapped to
    [t.width] on a terminal; [[]] if [d] is [None]. *)
