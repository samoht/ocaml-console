(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

type look = Look.t
(** The type for how a widget is drawn: in a theme, in UTF-8 or only ASCII, live
    or not. *)

val border : look -> Border.t option -> Border.t
(** [border look b] is [b] if given, otherwise the border of the theme of
    [look]. A border with no style is drawn in {!muted}. When [look] is not
    UTF-8 and one of the glyphs of the border is not ASCII, the border uses the
    glyphs of {!Border.ascii} in the same style. A glyph that the border leaves
    empty stays empty. *)

val guide : look -> Guide.t option -> Guide.t
(** [guide look g] is like {!border} for the guide of a tree. *)

val glyph : look -> string -> ascii:string -> string
(** [glyph look g ~ascii] is [g], or [ascii] when [look] is not UTF-8 and [g] is
    not ASCII. *)

val ascii_theme : look -> bool
(** [ascii_theme] is {!Look.ascii_theme}. *)

val line : look -> string -> string
(** [line look g] is [g], the glyph of a line. It is [-] when [look] is not
    UTF-8 and [g] is not ASCII, or when [look] is an {!ascii_theme}. *)

val spinner : look -> time:float -> string
(** [spinner look ~time] is the frame of the spinner of [look], [time] seconds
    in, as {!glyph} shows it. For ASCII, it is the frame of {!Spinner.ascii}. *)

val mark : look -> Theme.mark -> string
(** [mark look m] is the mark of [m] in the theme of [look], as {!glyph} shows
    it. For ASCII, it is the mark of {!Theme.ascii}. *)

val bar : look -> Theme.bar
(** [bar look] is the glyphs of a bar in [look]: the glyphs of its theme, or
    [`Ascii] when [look] is not UTF-8. *)

val ellipsis : look -> string
(** [ellipsis] is {!Look.ellipsis}. *)

val truncate : look -> int -> View.t -> View.t
(** [truncate look w s] is {!View.truncate}[ w s] with {!ellipsis}. *)

val muted : look -> Style.t
(** [muted look] is the style of the muted colour of the theme of [look]. *)

val accent : look -> Style.t
(** [accent look] is the style of the main colour of the theme of [look]. *)

val one_line : View.t -> View.t
(** [one_line s] is [s] with each line feed replaced by a space. *)

val cells : string -> int
(** [cells s] is the number of cells that [s] takes on a terminal. *)

val widest : string list -> int
(** [widest gs] is the {!cells} of the widest string of [gs], or [0] if [gs] is
    empty. *)

val fill : int -> string -> string
(** [fill n g] is [g] repeated over exactly [n] cells: as many copies of [g] as
    fit, then the characters of one more copy that fit, then spaces. It is [""]
    if [n] is not positive, and [n] spaces if [g] takes no cell. *)

val rails : Guide.t -> bool list -> string
(** [rails g lasts] is the guides beside a node. [lasts] holds the flags of the
    ancestors of the node, innermost first. A flag is [true] when the ancestor
    is the last of its siblings, and then the guide under it is a
    {!Guide.space}. Otherwise it is a {!Guide.pipe}. *)

val branch : Guide.t -> bool list -> string
(** [branch g (own :: up)] is the guide before a node with the flag [own] and
    the ancestor flags [up]: {!rails} of [up], then {!Guide.last} if [own],
    otherwise {!Guide.branch}. It is [""] for a root, whose flags are [[]]. *)

val pad_glyph : int -> string -> string
(** [pad_glyph n g] is [g] followed by the spaces that make it [n] cells wide.
*)

val pad : int -> View.t
(** [pad n] is [n] spaces, or no space if [n] is not positive. *)

val framed_kind : View.t list -> look -> View_core.kind
(** [framed_kind parts look] is how a frame around [parts] changes in [look]. If
    [look] is live and the frames of its theme move ({!Theme.val-motion}), the
    frame changes with the time. Otherwise it changes as its parts change. *)
