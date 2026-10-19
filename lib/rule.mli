(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal rules with optional styled labels.

    A rule is a {!Span.t}, so it renders and lays out as any span does. *)

type align = [ `Left | `Center | `Right ]
(** The type for label alignments within a rule. *)

val v :
  ?char_width:(Uchar.t -> int) ->
  ?theme:Theme.t ->
  ?style:Style.t ->
  ?glyph:string ->
  ?align:align ->
  ?label:Span.t ->
  width:int ->
  unit ->
  Span.t
(** [v ~width ()] is a [width]-cell horizontal rule. [glyph] defaults to U+2500
    (box drawings light horizontal), and must occupy exactly one terminal cell.
    A [label] is separated from the rule by spaces and aligned as requested. An
    explicit [style] wins; otherwise a theme supplies its accent colour and an
    unthemed rule is dim.

    @raise Invalid_argument
      if [width] is negative, [glyph] is not one safe terminal cell, or [label]
      is wider than [width]. *)
