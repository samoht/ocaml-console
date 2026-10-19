(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Printers side by side or one above the other.

    A block is any printer, such as [Fmt.const Panel.pp panel]: it is drawn at
    its natural width, in the colours the output takes, and its lines are joined
    with the other blocks' lines. *)

val hcat :
  ?gutter:int -> ?align:[ `Top | `Bottom ] -> unit Fmt.t list -> unit Fmt.t
(** [hcat blocks] prints [blocks] side by side, each padded to its widest line
    and the next one [gutter] spaces away (defaults to [1]). A block shorter
    than the others is aligned with their top (the default, [`Top]) or their
    bottom ([`Bottom]). A line ends at its last block that is not empty, with no
    spaces after it.

    @raise Invalid_argument if [gutter] is negative. *)

val vcat : ?gutter:int -> unit Fmt.t list -> unit Fmt.t
(** [vcat blocks] prints [blocks] one above the other, [gutter] blank lines
    apart (defaults to [0]).

    @raise Invalid_argument if [gutter] is negative. *)
