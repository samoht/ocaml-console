(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type scope = Display_core.scope
type level = Display_core.level

let scope = Display_core.scope

type form = [ `Plain | `Quiet | `Json ]

let v ?layout ?header ?size ?form ~scope ~now ppf =
  let form =
    Option.map
      (function
        | `Plain -> Display_state.Plain
        | `Quiet -> Display_state.Quiet
        | `Json -> Display_state.Json)
      form
  in
  Display_core.v ?layout ?header ?size ?form ~scope ~now ppf

let is_json = Display_event.is_json
let report = Display_core.report
let current = Display_core.current
