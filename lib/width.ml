(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_ascii s = String.for_all Char.Ascii.is_print s

let string_width s =
  if is_ascii s then String.length s
  else Glyph.String.measure ~width_method:`Unicode ~tab_width:1 s
