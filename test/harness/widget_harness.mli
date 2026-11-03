(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cell text and checks for the fuzz suites of console's widgets. *)

open Console

val pick : string -> int -> string
(** [pick s i] is a cell text chosen by byte [i] of [s], from texts whose bytes,
    clusters and cells do not line up. *)

val spell : string array -> string -> string
(** [spell alphabet s] is the text with an entry of [alphabet] for each byte of
    [s], chosen by that byte. *)

val room_width : int -> int
(** [room_width n] is a width of at most 200 cells, chosen by [n]. *)

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
(** [check_closed raw] checks that every style a line of [raw] turns on is off
    before that line ends. *)
