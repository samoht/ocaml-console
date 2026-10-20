(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

type look = { theme : Theme.t; utf_8 : bool }
(** The type for what a formatter says of how to draw a widget: its theme, and
    whether it shows UTF-8 or ASCII alone. *)

val look : Format.formatter -> look
(** [look ppf] is the look of the widgets printed on [ppf]: its {!Theme.get} and
    its {!Fmt.utf_8}. *)

val default_look : look
(** [default_look] is the look of a formatter given nothing: {!Theme.default},
    in UTF-8. *)

val border : look -> Border.t option -> Border.t
(** [border look b] is [b] if given, else the border of [look]'s theme, in
    {!muted} if it has no style; in {!Border.ascii}, in the same style, when
    [look] is not UTF-8 and one of its glyphs is not ASCII. *)

val guide : look -> Guide.t option -> Guide.t
(** [guide look g] is like {!border} for the guide of a tree. *)

val glyph : look -> string -> ascii:string -> string
(** [glyph look g ~ascii] is [g], or [ascii] when [look] is not UTF-8 and [g] is
    not ASCII. *)

val spinner : look -> elapsed:float -> string
(** [spinner look ~elapsed] is the frame of [look]'s spinner [elapsed] seconds
    in, as {!glyph} shows it, with {!Spinner.ascii}'s frame for ASCII. *)

val mark : look -> Theme.mark -> string
(** [mark look m] is the mark of [m] in [look]'s theme, as {!glyph} shows it,
    with {!Theme.ascii}'s mark for ASCII. *)

val bar : look -> Theme.bar
(** [bar look] is the glyphs of a bar in [look]: its theme's, or [`Ascii] when
    [look] is not UTF-8. *)

val ellipsis : look -> string
(** [ellipsis look] ends a cut line: U+2026 in UTF-8, three dots in ASCII. *)

val truncate : look -> int -> Span.t -> Span.t
(** [truncate look w s] is {!Span.truncate}[ w s] with {!ellipsis}. *)

val muted : look -> Style.t
(** [muted look] is the style of the muted colour of [look]'s theme. *)

val accent : look -> Style.t
(** [accent look] is the style of the main colour of [look]'s theme. *)

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

val pp : (look -> margin:int -> 'a -> Span.t list) -> 'a Fmt.t
(** [pp lines] formats the [lines] of a widget in the formatter's {!look} and at
    its margin, one per line, with no line end after the last. *)

val to_string :
  (look -> margin:int -> 'a -> Span.t list) ->
  ?styled:bool ->
  ?width:int ->
  'a ->
  string
(** [to_string lines v] is the [lines] of [v] in {!default_look} at a margin of
    [width] (defaults to [max_int]: its natural width), joined by line feeds,
    styled iff [styled] (defaults to [false]). *)
