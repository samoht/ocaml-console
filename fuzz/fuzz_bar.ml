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
  match Bar.horizontal ~brackets:b ~width f with
  | bar ->
      if width < 0 then fail "negative width accepted";
      check_eq ~pp:Fmt.int
        (width + if b then 2 else 0)
        (Width.string_width (View.to_string bar))
  | exception Invalid_argument _ -> if width >= 0 then fail "width refused"

(* A vertical bar is one cell wide and exactly as tall as its room, at any
   fraction. *)
let test_vertical_height h frac utf_8 =
  let height = 1 + (abs h mod 40) in
  let s =
    Display_harness.at ~utf_8 ~time:0. (View.sized ~height (Bar.vertical frac))
  in
  let lines = String.split_on_char '\n' s in
  check_eq ~pp:Fmt.int height (List.length lines);
  List.iter (fun l -> check_eq ~pp:Fmt.int 1 (View.width (View.text l))) lines

let suite =
  ( "bar",
    [
      test_case "width" [ int; float; bool ] test_width;
      test_case "a vertical bar is as tall as its room" [ int; float; bool ]
        test_vertical_height;
    ] )
