(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* A bar is exactly [width] cells, two more with brackets, for any fraction;
   a negative width is refused. *)
let test_width n f b =
  let width = n mod 100 in
  match Bar.v ~brackets:b ~width f with
  | bar ->
      if width < 0 then fail "negative width accepted";
      check_eq ~pp:Fmt.int (width + if b then 2 else 0) (Span.width bar)
  | exception Invalid_argument _ -> if width >= 0 then fail "width refused"

let suite = ("bar", [ test_case "width" [ int; float; bool ] test_width ])
