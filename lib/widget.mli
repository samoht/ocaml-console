(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val border :
  ?theme:Theme.t -> ?border:Border.t -> default:Border.t -> unit -> Border.t

val animated : Theme.t option -> bool
