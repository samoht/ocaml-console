(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cell text and checks for the fuzz suites of console's widgets. *)

open Console

val pick : string -> int -> string
(** [pick s i] is a cell text chosen by byte [i] of [s], from texts whose bytes,
    clusters and cells do not line up. *)

val nth_theme : int -> Theme.t
(** [nth_theme k] is theme [k] of {!Console.Theme.all}, wrapping round, for a
    [k] that is not negative. *)

val render :
  ?theme:Theme.t ->
  (Format.formatter -> 'a -> unit) ->
  int ->
  'a ->
  string * string list
(** [render pp margin v] is what [pp] prints of [v] at [margin], in colour, in
    [theme] if given: the bytes, and their lines without their escapes. *)

val cells : string -> int
(** [cells s] is the width of [s] in terminal cells. *)

val check_fits : int -> string list -> unit
(** [check_fits margin lines] checks that no line of [lines] is wider than
    [margin]. *)

val check_closed : string -> unit
(** [check_closed raw] checks that each line of [raw] ends with no style on and
    no link open: an SGR other than a reset turns a style on, and an OSC 8 with
    a target opens a link. *)
