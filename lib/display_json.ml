(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let hex = "0123456789abcdef"

let add_char buf u =
  match Uchar.to_int u with
  | 0x22 -> Buffer.add_string buf "\\\""
  | 0x5c -> Buffer.add_string buf "\\\\"
  | 0x0a -> Buffer.add_string buf "\\n"
  | 0x0d -> Buffer.add_string buf "\\r"
  | 0x09 -> Buffer.add_string buf "\\t"
  | c when c < 0x20 || (c >= 0x7f && c <= 0x9f) ->
      (* Below U+00A0: two hexadecimal digits. *)
      Buffer.add_string buf "\\u00";
      Buffer.add_char buf hex.[c lsr 4];
      Buffer.add_char buf hex.[c land 15]
  | _ -> Buffer.add_utf_8_uchar buf u

let add_string buf s =
  Buffer.add_char buf '"';
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      add_char buf
        (if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
         else Uchar.rep);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.add_char buf '"'

let add_number buf x =
  Buffer.add_string buf (if Float.is_finite x then Fmt.str "%.3f" x else "0")
