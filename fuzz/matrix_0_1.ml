(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let on_matrix_0_1 = String.starts_with ~prefix:"0.1." Matrix_version.v
let discard_if b = if on_matrix_0_1 && b then Alcobar.bad_test ()

(* VS16 is EF B8 8F in UTF-8. *)
let has_vs16 c =
  let rec from i =
    i + 3 <= String.length c
    && (String.sub c i 3 = "\xef\xb8\x8f" || from (i + 1))
  in
  from 0

(* matrix 0.1.0 draws a cluster with a VS16 two cells wide, whatever console
   counts. *)
let discard_stray_vs16 s =
  discard_if
    (List.exists
       (fun c -> has_vs16 c && Width.string_width c <> 2)
       (Width.clusters s))

let is_printable_ascii c = c >= ' ' && c <= '~'

let discard_dropped_mark s =
  let rec from i =
    if i >= String.length s then false
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      i > 0
      && is_printable_ascii s.[i - 1]
      && n > 1
      && Width.string_width (String.sub s i n) = 0
      || from (i + n)
  in
  discard_if (from 0)
