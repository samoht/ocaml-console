(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Formatters that capture what is printed on them as spans, as
    [Span_core.capture] makes them, for the scene of a display. *)

type Format.stag +=
  | Styled of Color.t Sgr.style
        (** The style the text up to the matching close is in, the styles of the
            enclosing [Style.styled] calls added. *)

val set : Format.formatter -> unit
(** [set ppf] makes [ppf] a formatter that captures. *)

val is : Format.formatter -> bool
(** [is ppf] is [true] iff {!set} made [ppf] a formatter that captures. *)
