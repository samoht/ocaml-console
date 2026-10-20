(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

val border : ?theme:Theme.t -> ?border:Border.t -> unit -> Border.t
(** [border ~theme ~border ()] is [border] if given, else the border of [theme]
    (defaults to {!Theme.default}). *)

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

val rails : Guide.t -> bool list -> string
(** [rails g lasts] are the guides beside a node whose ancestors' flags,
    innermost first, are [lasts]: [true] when the ancestor is the last of its
    siblings, which leaves a {!Guide.space} under it, else a {!Guide.pipe}. *)

val branch : Guide.t -> bool list -> string
(** [branch g (own :: up)] is the guide before a node whose flag is [own] and
    whose ancestors' are [up]: {!rails} of [up], then {!Guide.last} if [own]
    else {!Guide.branch}. It is [""] for a root, whose flags are [[]]. *)

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
