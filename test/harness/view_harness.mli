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
(** [at ~time v] is the text of [v] as a display draws it [time] seconds after
    the start. It uses [theme] (defaults to {!Theme.default}), UTF-8 iff [utf_8]
    (defaults to [true]), and is live iff [live] (defaults to [true]). If
    [width] is given, the room is [width] wide. *)

val laid : ?live:bool -> ?tasks:bool -> unit -> View.t * int ref
(** [laid ()] is a view and the number of times it was laid out. The view is as
    wide as its room, and three cells wide at its natural size. It reads state
    outside the view iff [live] (defaults to [false]), and reads the tasks iff
    [tasks] (defaults to [false]). *)

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
    room with [glyph]. Without a room, it is one [glyph] wide. *)

val frame : string -> Border.t
(** [frame glyph] is the border drawn with [glyph] for every piece.
    @raise Invalid_argument as {!Console.Border.v} does. *)

val small_table : border:Border.t -> View.t
(** [small_table ~border] is a table in [border] of two columns, [A] and [B],
    and a row, [x] and [y]. *)
