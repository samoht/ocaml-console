(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_ascii () =
  let border = Border.ascii in
  Alcotest.(check string) "ascii top_left" "+" (Border.chars border).top_left

let test_unicode () =
  let border = Border.rounded in
  Alcotest.(check string) "rounded top_left" "╭" (Border.chars border).top_left

(* A table's inner lines are the frame's own in every preset but [dos],
   whose frame is double and whose inner lines are single. *)
let test_inner_lines () =
  List.iter
    (fun (name, b) ->
      let c = Border.chars b in
      Alcotest.(check string) (name ^ " vertical") c.left c.vertical;
      Alcotest.(check string) (name ^ " horizontal") c.top c.horizontal)
    Border.
      [
        ("none", none);
        ("ascii", ascii);
        ("single", single);
        ("double", double);
        ("rounded", rounded);
        ("heavy", heavy);
        ("hidden", hidden);
      ];
  let c = Border.chars Border.dos in
  Alcotest.(check (list string))
    "dos"
    [
      "\u{2554}";
      "\u{2550}";
      "\u{2557}";
      "\u{2551}";
      "\u{2551}";
      "\u{255a}";
      "\u{2550}";
      "\u{255d}";
      "\u{2502}";
      "\u{2500}";
      "\u{253c}";
      "\u{2564}";
      "\u{2567}";
      "\u{255f}";
      "\u{2562}";
    ]
    [
      c.top_left;
      c.top;
      c.top_right;
      c.left;
      c.right;
      c.bottom_left;
      c.bottom;
      c.bottom_right;
      c.vertical;
      c.horizontal;
      c.cross;
      c.top_cross;
      c.bottom_cross;
      c.left_cross;
      c.right_cross;
    ]

let suite =
  ( "border",
    [
      Alcotest.test_case "inner lines" `Quick test_inner_lines;
      Alcotest.test_case "ascii" `Quick test_ascii;
      Alcotest.test_case "unicode" `Quick test_unicode;
    ] )
