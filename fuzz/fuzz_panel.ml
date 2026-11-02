(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console
open Widget_harness

(* A panel is no wider than the margin, turns its styles off, and a panel of
   fixed width that fits the margin is a rectangle of that width. *)
let test_fits s n m =
  let margin = 1 + (abs m mod 80) in
  let width = 4 + (abs n mod 40) in
  let content = View.text (pick s 0 ^ "\n" ^ pick s 1) in
  let title = View.text (pick s 2) in
  List.iter
    (fun p ->
      let raw, lines = render View.pp margin p in
      check_fits margin lines;
      check_closed raw)
    [
      Panel.view ~title content;
      View.sized ~width (Panel.view ~expand:true ~title content);
    ];
  if width <= margin then
    let _, lines =
      render View.pp margin
        (View.sized ~width (Panel.view ~expand:true ~title content))
    in
    List.iter (fun l -> check_eq ~pp:Fmt.int width (cells l)) lines

let test_refused n =
  match Panel.view ~padding:(n mod 3) View.empty with
  | _ -> if n mod 3 < 0 then fail "accepted"
  | exception Invalid_argument _ -> if n mod 3 >= 0 then fail "refused"

let test_wide_border n =
  let glyph = String.make (2 + (n mod 3)) '#' in
  let border =
    Border.v
      Border.
        {
          top_left = glyph;
          top = glyph;
          top_right = glyph;
          left = glyph;
          right = glyph;
          bottom_left = glyph;
          bottom = glyph;
          bottom_right = glyph;
          vertical = glyph;
          horizontal = glyph;
          cross = glyph;
          top_cross = glyph;
          bottom_cross = glyph;
          left_cross = glyph;
          right_cross = glyph;
        }
  in
  let width = 20 in
  let out =
    View.to_string
      (View.sized ~width (Panel.view ~border ~expand:true (View.text "x")))
  in
  List.iter
    (fun l -> check_eq ~pp:Fmt.int width (cells l))
    (String.split_on_char '\n' out)

(* Compare before multiplying: no finite box can hold padding this large,
   whether it expands to its room or follows its content. *)
let test_overflowing_padding n expand =
  let padding = max_int - (n mod 1024) in
  match Panel.view ~padding ~expand View.empty with
  | _ -> failf "accepted padding %d" padding
  | exception Invalid_argument _ -> ()

let suite =
  ( "panel",
    [
      test_case "overflowing padding is refused at construction" [ uint8; bool ]
        test_overflowing_padding;
      test_case "multi-cell borders respect fixed width" [ uint8 ]
        test_wide_border;
      test_case "fits the margin" [ bytes; int; int ] test_fits;
      test_case "refuses a box too small" [ int ] test_refused;
    ] )
