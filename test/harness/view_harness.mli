(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Views laid out as a display lays them out, for the tests of console's
    widgets. *)

open Console

val at :
  ?theme:Theme.t ->
  ?utf_8:bool ->
  ?live:bool ->
  ?width:int ->
  time:float ->
  View.t ->
  string
(** [at ~time v] is the text of [v] laid out as a display draws it [time]
    seconds in, in [theme] (defaults to {!Theme.default}), in UTF-8 iff [utf_8]
    (defaults to [true]), live iff [live] (defaults to [true]), in a room
    [width] wide if given. *)

val laid : ?live:bool -> ?tasks:bool -> unit -> View.t * int ref
(** [laid ()] is a view as wide as its room, three cells at its natural size,
    which reads state outside the view iff [live] (defaults to [false]), the
    tasks iff [tasks] (defaults to [false]), and the count of the times it was
    laid out. *)

val capture : (Format.formatter -> unit) -> string
(** [capture f] is what [f] prints on a formatter, once flushed. *)

val printed :
  ?theme:Theme.t ->
  ?utf_8:bool ->
  ?margin:int ->
  ?depth:Color.depth ->
  View.t ->
  string
(** [printed v] is what {!Console.View.pp} prints of [v] on a formatter set up
    with the arguments given: [theme], [utf_8], [margin], and colour at [depth].
    Without [depth] it prints no colour. *)

val sgr : Style.t -> string
(** [sgr style] is the escape code a span in [style] starts with. *)

val fill : char -> int -> View.t
(** [fill glyph grow] is a view of weight [grow] that fills the width of its
    room with [glyph], or is one [glyph] wide without one. *)

val frame : string -> Border.t
(** [frame glyph] is the border drawn with [glyph] for every piece.
    @raise Invalid_argument as {!Console.Border.v} does. *)

val small_table : border:Border.t -> View.t
(** [small_table ~border] is a table in [border] of two columns, [A] and [B],
    and a row, [x] and [y]. *)
