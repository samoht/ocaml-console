(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

val border : ?theme:Theme.t -> ?border:Border.t -> unit -> Border.t
(** [border ~theme ~border ()] is [border] if given, else the border of [theme]
    (defaults to {!Theme.unicode}). *)

val one_line : Span.t -> Span.t
(** [one_line s] is [s] with each line feed a space. *)

val cells : string -> int
(** [cells s] is the number of cells [s] takes on a terminal. *)

val widest : string list -> int
(** [widest gs] is the {!cells} of the widest of [gs], [0] if there is none. *)

val fill : int -> string -> string
(** [fill n g] is [g] repeated over exactly [n] cells: the copies of [g] that
    fit, then the characters of one more that fit, then spaces. It is [""] if
    [n] is not positive, and [n] spaces if [g] takes no cell. *)

val pad_glyph : int -> string -> string
(** [pad_glyph n g] is [g] followed by the spaces that make it [n] cells wide.
*)

val pad : int -> Span.t
(** [pad n] is [n] spaces, none if [n] is not positive. *)

val pp : (margin:int -> 'a -> Span.t list) -> 'a Fmt.t
(** [pp lines] formats the [lines] of a widget at the formatter's margin, one
    per line, with no line end after the last. *)

val to_string :
  (margin:int -> 'a -> Span.t list) ->
  ?styled:bool ->
  ?width:int ->
  'a ->
  string
(** [to_string lines v] is the [lines] of [v] at a margin of [width] (defaults
    to [max_int]: its natural width), joined by line feeds, styled iff [styled]
    (defaults to [false]). *)
