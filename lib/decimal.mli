(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Numbers with a fixed count of decimals, the same on every platform.

    [Printf]'s [%.1f] leaves the rounding to the C library, which differs: glibc
    rounds [4.25] to ["4.2"] and MinGW's to ["4.3"]. These functions round
    themselves, so a program prints the same digits everywhere. *)

val string_of_float : digits:int -> float -> string
(** [string_of_float ~digits x] is [x] rounded to [digits] decimals, a tie away
    from zero, such as ["4.3"] for [string_of_float ~digits:1 4.25] and ["12"]
    for [string_of_float ~digits:0 11.5]. A value that rounds to zero has no
    sign. NaN is ["nan"] and the infinities are ["inf"] and ["-inf"]. Beyond
    [2{^62}] units of the last decimal, the digits are [Printf]'s.

    @raise Invalid_argument if [digits] is negative. *)

val pp : digits:int -> float Fmt.t
(** [pp ~digits] formats as {!string_of_float}[ ~digits]. *)
