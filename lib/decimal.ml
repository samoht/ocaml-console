(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rec scale d = if d = 0 then 1. else 10. *. scale (d - 1)

(* The digits come from an integer count of the last decimal, so no C
   library rounds them. *)
let string_of_float ~digits x =
  if digits < 0 then
    invalid_arg "Console.Decimal.string_of_float: negative digits";
  if Float.is_nan x then "nan"
  else if x = Float.infinity then "inf"
  else if x = Float.neg_infinity then "-inf"
  else
    let n = Float.round (Float.abs x *. scale digits) in
    if n >= 0x1p62 then Fmt.str "%.*f" digits x
    else
      let n = int_of_float n in
      let s = string_of_int n in
      let s =
        if String.length s > digits then s
        else String.make (digits + 1 - String.length s) '0' ^ s
      in
      let i = String.length s - digits in
      let s =
        if digits = 0 then s else String.sub s 0 i ^ "." ^ String.sub s i digits
      in
      if x < 0. && n > 0 then "-" ^ s else s

let pp ~digits ppf x = Fmt.string ppf (string_of_float ~digits x)
