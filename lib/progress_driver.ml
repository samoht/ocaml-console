(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let show ~write_error ?cancelled ?atomic =
  Progress_show.show_with ~write_error ?cancelled ?atomic
