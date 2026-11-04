(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

let silent =
  List.filter
    (fun c -> not (List.mem c [ 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x1b ]))
    (List.init 0x20 Fun.id)
  @ [ 0x7f ]
