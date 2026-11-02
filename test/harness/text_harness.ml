(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let silent =
  List.filter
    (fun c -> not (List.mem c [ 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x1b ]))
    (List.init 0x20 Fun.id)
  @ [ 0x7f ]
