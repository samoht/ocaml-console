(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type Format.stag += Styled of Color.t Sgr.style

let slot = Slot.v ()
let set ppf = Slot.set slot ppf ()
let is ppf = Option.is_some (Slot.find slot ppf)
