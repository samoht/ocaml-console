(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

let maximal_subparts =
  let r n = String.concat "" (List.init n (fun _ -> "\u{FFFD}")) in
  [
    ("table 3-8", "\xc0\xaf\xe0\x80\xbf\xf0\x81\x82A", r 8 ^ "A");
    ("table 3-9", "\xed\xa0\x80\xed\xbf\xbf\xed\xafA", r 8 ^ "A");
    ("table 3-10", "\xf4\x91\x92\x93\xffA\x80\xbfB", r 5 ^ "A" ^ r 2 ^ "B");
    ("table 3-11", "\xe1\x80\xe2\xf0\x91\x92\xf1\xbfA", r 4 ^ "A");
  ]

let silent =
  List.filter
    (fun c -> not (List.mem c [ 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x1b ]))
    (List.init 0x20 Fun.id)
  @ [ 0x7f ]
