(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console
open Widget_harness

(* A tree is no wider than the margin, and shows each node on a line of its
   own when the margin leaves room for every label. *)
let test_fits s n =
  let margin = 1 + (abs n mod 80) in
  let leaf i = Tree.v (View.text (pick s i)) [] in
  let tree =
    Tree.v
      (View.text (pick s 0))
      [ Tree.v (View.text (pick s 1)) [ leaf 2; leaf 3 ]; leaf 4 ]
  in
  let raw, lines = render View.pp margin (Tree.view tree) in
  check_fits margin lines;
  check_closed raw;
  if margin >= 80 then check_eq ~pp:Fmt.int 5 (List.length lines)

let suite = ("tree", [ test_case "fits the margin" [ bytes; int ] test_fits ])
