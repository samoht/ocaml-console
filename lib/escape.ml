(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let sgr depth (style : Color.t Sgr.style) =
  let down = Option.map (Color.downsample depth) in
  Sgr.start { style with fg = down style.fg; bg = down style.bg }
