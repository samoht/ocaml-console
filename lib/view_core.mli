(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Views, with the operations that the widgets and the display share. {!View}
    is the public interface and documents the values that both modules have. *)

type t
(** The type for views. *)

type align = [ `Left | `Center | `Right ]
(** See {!View.align}. *)

(** {1:room Room and time} *)

type kind = { live : bool; tasks : bool; timed : bool }
(** The type for what a deferred view reads besides its room. [live] is state
    outside the view that changes between frames. [tasks] is the tasks, which
    are part of that state. [timed] is the time. A view that reads none of them
    caches its layout per room. *)

val static : kind
(** [static] reads nothing besides the room. *)

val timed : kind
(** [timed] reads only the time. *)

val tasks : kind
(** [tasks] reads the tasks and the time, and no other state. *)

val moving : kind
(** [moving] reads any state and the time. *)

val join : kind -> kind -> kind
(** [join k0 k1] reads what [k0] or [k1] reads. *)

val deferred :
  ?kind:(Look.t -> kind) ->
  ?grow:int ->
  ?min_width:int ->
  ?still:bool ->
  (Ctx.t -> t) ->
  t
(** [deferred ~kind ~grow ~min_width ~still lay] is the view [lay c] when it is
    laid out in [c].
    - [kind] (defaults to {!static} in every look) says what the view reads.
    - [grow] (defaults to [0]) is its weight in an {!hcat} that shares a room.
    - [min_width] is the width below which it should not be laid out.
    - With [still] (defaults to [false]), every frame has as many lines as the
      frame at time [0.], and each line is cut or padded to the width of the
      same line at time [0.]. *)

val kind : t -> Look.t -> kind
(** [kind v look] is what [v] reads in [look]: all that its parts read. *)

val kinds : t list -> Look.t -> kind
(** [kinds vs look] is what any of [vs] reads in [look]. *)

val is_flat : t -> bool
(** [is_flat v] is [true] iff [v] has no deferred part. *)

val min_width : Ctx.t -> t -> int
(** [min_width c v] is the width below which [v] should not be laid out in [c].
    For a flat view, it is the width of its widest word. For a deferred view, it
    is the [min_width] given to it, or else the [min_width] of the view it lays
    out in [c]. *)

val resolve : Ctx.t -> t -> t
(** [resolve c v] is [v] laid out in [c], with no deferred part. A {!drawn} part
    stays drawn. *)

(** {1:views Views} *)

val empty : t
(** [empty] is {!View.empty}. *)

val is_empty : t -> bool
(** [is_empty v] is [true] iff [v] has no text at its natural size. *)

val text : string -> t
(** [text s] is {!View.text}. *)

val ink : Style.t -> string -> t
(** [ink style s] is [styled style (text s)]. *)

val link : uri:string -> t -> t
(** [link ~uri v] is {!View.link}. *)

val styled : Style.t -> t -> t
(** [styled base v] is {!View.styled}. *)

val role : Theme.role -> t -> t
(** [role r v] is {!View.role}. *)

val gradient : ?colors:Color.t list -> t -> t
(** [gradient ~colors v] is {!View.gradient}. *)

val ( ++ ) : t -> t -> t
(** [v0 ++ v1] is [v0] followed by [v1]. *)

val concat : ?sep:t -> t list -> t
(** [concat ~sep views] is {!View.concat}. *)

val hcat : ?gap:int -> t list -> t
(** [hcat ~gap views] is {!View.hcat}. *)

val vcat : ?gap:int -> t list -> t
(** [vcat ~gap views] is {!View.vcat}. *)

val fit : ?min_width:int -> ?grow:int -> (Ctx.t -> t) -> t
(** [fit ~min_width ~grow f] is {!View.fit}. *)

val sized : ?width:int -> ?height:int -> t -> t
(** [sized ~width ~height v] is {!View.sized}. *)

val place : ?align:align -> ?width:int -> ?height:int -> t -> t
(** [place ~align ~width ~height v] is {!View.place}. *)

val width : t -> int
(** [width v] is {!View.width}. *)

val height : t -> int
(** [height v] is {!View.height}. *)

val ellipsis : string
(** [ellipsis] is U+2026. *)

val truncate : ?ellipsis:string -> int -> t -> t
(** [truncate ~ellipsis w v] is like {!View.truncate}, but ends a cut line with
    [ellipsis] (defaults to the {!Look.ellipsis} of the look it is printed in).
    If [ellipsis] is wider than [w] cells, only its first grapheme clusters that
    fit in [w] cells are kept. *)

val crop : ?left:int -> ?width:int -> t -> t
(** [crop ~left ~width v] is {!View.crop}. *)

val wrap : ?words:(string -> string list) -> int -> t -> t list
(** [wrap ~words w v] is {!View.wrap}. *)

val pp : t Fmt.t
(** [pp] is {!View.pp}. *)

val to_string : ?styled:bool -> t -> string
(** [to_string ~styled v] is {!View.to_string}. *)

(** {1:widgets For the widgets}

    When given a deferred view, these functions lay it out at its natural size,
    in {!Look.default}, at time [0.]. *)

val lines : t -> t list
(** [lines v] are the lines of [v], without their line feeds. It is never empty.
*)

val block : t list -> t
(** [block lines] is [lines] one under the other, as one block. When the region
    of a display is too short for a whole view, it drops all the lines of a
    block together. *)

val seamed : t -> (bool * t) list
(** [seamed v] is the {!lines} of [v], each paired with [true] iff it is in the
    same {!block} as the line above it. *)

val pad_to : int -> t -> t
(** [pad_to w line] is [line] cut to its first [w] cells, between two clusters,
    then padded with spaces to exactly [w] cells. *)

val wrap_hanging : int -> t -> t list
(** [wrap_hanging w v] is like {!wrap}, but the continuation lines of each line
    start under the text after its first word, at most [w / 2] cells in. *)

val chop : int -> t -> t list
(** [chop w v] is like {!wrap}, but breaks each line of [v] every [w] cells,
    even inside a word, as a terminal wraps a line that is too wide. *)

val widest_word : t -> int
(** [widest_word v] is the width of the widest run of [v] without a space. *)

val drawn : (Look.t -> t) -> t
(** [drawn draw] is the view [draw look] when it is printed in [look]. It is
    measured as [draw Look.default]. Every [draw look] must take the same cells
    as [draw Look.default], one cluster for one. When {!truncate} or {!wrap}
    keep a part of the view, that part is drawn as the same cells of
    [draw look]. *)

val paint : Look.t -> t -> t
(** [paint look v] is [v] with each {!drawn} part drawn in [look]. *)

val recell :
  (cell:int -> (Style.t -> string -> Style.t * string) option) -> t -> t
(** [recell f line] is [line] with some clusters redrawn. Let [g] be a cluster
    in style [s] that starts [cell] cells into [line]. When [f ~cell] is
    [Some r], [g] is drawn as [r s g], a new style and glyph. If the new glyph
    does not take the same cells as [g], or holds a control character, the new
    style is drawn with the glyph [g]. A cluster of a {!drawn} part is never
    redrawn. *)

val render : look:Look.t -> ?depth:Color.depth -> t -> string
(** [render ~look ~depth v] is [v] drawn in [look], with its styles at [depth].
    Without [depth], it is only the text of [v]. *)

val pp_lines : t list Fmt.t
(** [pp_lines] formats views one per line, with no line end after the last one.
*)

val string_of_lines : ?styled:bool -> t list -> string
(** [string_of_lines lines] is [lines] joined by line feeds, each as
    {!to_string} gives it. *)
