(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal lines with an optional label. *)

type align = [ `Left | `Center | `Right ]
(** The type for label alignments within a line. *)

type t
(** The type for horizontal lines. *)

val v :
  ?theme:Theme.t ->
  ?style:Style.t ->
  ?glyph:string ->
  ?align:align ->
  ?label:Span.t ->
  unit ->
  t
(** [v ()] is a horizontal line drawn with [glyph] (defaults to a light
    horizontal line, U+2500) in [style] (defaults to the accent colour of
    [theme], or faint without one). [label], if given, sits in the line at
    [align] (defaults to [`Center]), with a space on each side.

    @raise Invalid_argument
      if [glyph], with each control character a space, is not one terminal cell
      wide. *)

val pp : t Fmt.t
(** [pp] formats a line exactly as wide as the formatter's margin. A label wider
    than that is cut with an ellipsis. *)
