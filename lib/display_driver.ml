(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type scope = Display_core.scope
type level = Display_core.level

let scope = Display_core.scope

let v ?layout ?summary ?view ?keep_ended ?size ?progress ~scope ~now ppf =
  Display_core.v ?layout ?summary ?view ?keep_ended ?size ?progress ~scope ~now
    ppf

let is_json = Display_event.is_json
let ending (t : Display.t) f = t.Display_state.ending <- f
let report = Display_core.report
let current = Display_core.current
let watch = Display_core.watch
