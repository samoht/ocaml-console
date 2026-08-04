(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let str = Alcotest.(check string)

(* The guide prefix is the standard tree drawing: each strict ancestor is a
   vertical bar (if it has a following sibling) or blank (if last), then the
   node's own connector. *)
let test_prefix_unicode () =
  str "root" "" (Tree.prefix []);
  str "only/first top-level" "├── " (Tree.prefix [ false ]);
  str "last top-level" "└── " (Tree.prefix [ true ]);
  str "child of non-last parent" "│   ├── " (Tree.prefix [ false; false ]);
  str "last child of non-last parent" "│   └── " (Tree.prefix [ false; true ]);
  str "child of last parent" "    ├── " (Tree.prefix [ true; false ]);
  str "last child of last parent" "    └── " (Tree.prefix [ true; true ]);
  str "three levels, mixed" "│       ├── " (Tree.prefix [ false; true; false ])

let test_prefix_ascii () =
  let guide = Guide.ascii in
  str "ascii last top-level" "+-- " (Tree.prefix ~guide [ true ]);
  str "ascii child of non-last" "|   +-- " (Tree.prefix ~guide [ false; false ]);
  str "ascii child of last" "    +-- " (Tree.prefix ~guide [ true; false ])

(* A real multi-level tree renders one node per line with correct guides. This
   is the exact rendering the previous implementation got wrong (everything on
   one line, ancestor branches repeated). *)
let test_render_exact () =
  let n label children = Tree.Node (Span.text label, children) in
  let tree =
    Tree.of_tree
      (n "root" [ n "a" [ n "a1" []; n "a2" [] ]; n "b" [ n "b1" [] ] ])
  in
  let expected =
    String.concat "\n"
      [ "root"; "├── a"; "│   ├── a1"; "│   └── a2"; "└── b"; "    └── b1" ]
  in
  str "exact tree" expected (Tree.to_string tree)

let test_render_single () =
  let tree =
    Tree.of_tree
      (Tree.Node (Span.text "root", [ Tree.Node (Span.text "child", []) ]))
  in
  str "single child" "root\n└── child" (Tree.to_string tree)

(* A tree animation is still: every frame is its rendered string. *)
let test_anim_const () =
  let t =
    Tree.of_tree
      (Tree.Node (Span.text "root", [ Tree.Node (Span.text "leaf", []) ]))
  in
  str "frame is terminal-ready" (Tree.to_ansi_string t)
    (Anim.frame (Tree.anim t) ~elapsed:3.0)

let suite =
  ( "tree",
    [
      Alcotest.test_case "prefix unicode" `Quick test_prefix_unicode;
      Alcotest.test_case "prefix ascii" `Quick test_prefix_ascii;
      Alcotest.test_case "render exact" `Quick test_render_exact;
      Alcotest.test_case "render single" `Quick test_render_single;
      Alcotest.test_case "anim is still" `Quick test_anim_const;
    ] )
