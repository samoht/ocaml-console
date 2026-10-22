(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Display_core

let v ?layout ?header ?size ?progress ~now ppf =
  Display_core.v ?layout ?header ?size ?progress ~now ppf
