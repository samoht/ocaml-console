(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let watch = Display_core.watch
let when_idle = Display_core.when_idle
let closed = Display_core.closed
let close t ~before = Display_core.close ~before t
let close_quietly = Display_core.close_quietly
let protect = Display_core.protect
let is_async = Display_core.is_async
