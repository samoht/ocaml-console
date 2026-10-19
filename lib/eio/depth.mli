(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour depth on a formatter.

    The core writes every colour as given. A terminal that shows fewer colours
    is served by rewriting, on the way out, the colours of each SGR sequence
    written to its formatter. *)

val sgr : Console.Color.depth -> string -> string
(** [sgr d s] is [s] with the 256-colour and 24-bit colour parameters of each
    complete SGR sequence replaced by those of {!Console.Color.downsample}[ d].
    Everything else is unchanged. *)

val install : Format.formatter -> Console.Color.depth -> unit
(** [install ppf d] makes [ppf] write its output through {!sgr}[ d]. A sequence
    cut between two writes is held until it is complete or [ppf] is flushed.
    Installing on a formatter already installed on changes nothing. *)
