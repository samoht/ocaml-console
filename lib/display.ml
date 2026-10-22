(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Display_core

type view = t -> width:int -> height:int -> View_core.t

let v ?layout ?summary ?view ?keep_ended ?size ?progress ~now ppf =
  Display_core.v ?layout ?summary ?view ?keep_ended ?size ?progress ~now ppf
