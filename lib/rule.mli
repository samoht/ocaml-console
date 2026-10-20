(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal lines with an optional label. *)

(** {1:rules Rules} *)

type align = [ `Left | `Center | `Right ]
(** The type for label alignments within a line. *)

type t
(** The type for horizontal lines. *)

val v :
  ?style:Style.t -> ?glyph:string -> ?align:align -> ?label:Span.t -> unit -> t
(** [v ()] is a horizontal line drawn with [glyph] (defaults to a light
    horizontal line, U+2500, and [-] for a [glyph] that is not ASCII on a
    formatter that is not {!Fmt.utf_8}) in [style] (defaults to the muted colour
    of the theme of the formatter it is printed on, {!Theme.get}). [label], if
    given, sits in the line at [align] (defaults to [`Center]), with a space on
    each side.

    @raise Invalid_argument
      if [glyph], with each control character a space, is not one terminal cell
      wide. *)

(** {1:converting Converting} *)

val to_string : ?styled:bool -> width:int -> t -> string
(** [to_string ~width rule] is [rule] as {!pp} formats it at a margin of
    [width], with colours iff [styled] (defaults to [false]). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a line exactly as wide as the formatter's margin. A label wider
    than that is cut with an ellipsis. *)
