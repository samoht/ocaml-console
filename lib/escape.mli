(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styles as the escape codes of a colour depth. *)

val sgr : Color.depth -> Color.t Sgr.style -> string
(** [sgr d s] is the escape code that selects [s], its colours
    {!Color.downsample}d to [d], or [""] if [s] sets nothing. *)
