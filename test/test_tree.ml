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

(* A multi-level tree renders one node per line, each ancestor's branch drawn
   once. *)
let test_render_exact () =
  let n label children = Tree.v (Span.text label) children in
  let tree = n "root" [ n "a" [ n "a1" []; n "a2" [] ]; n "b" [ n "b1" [] ] ] in
  let expected =
    String.concat "\n"
      [ "root"; "├── a"; "│   ├── a1"; "│   └── a2"; "└── b"; "    └── b1" ]
  in
  str "exact tree" expected (Tree.to_string tree)

let test_render_single () =
  let tree = Tree.v (Span.text "root") [ Tree.v (Span.text "child") [] ] in
  str "single child" "root\n└── child" (Tree.to_string tree)

(* The formatter's margin is a ceiling on every row: a label wider than the
   room left by its guide wraps, hanging under the text after its first word,
   and the guide of its level runs on beside the continuation. *)
let test_margin_wraps () =
  let n label children = Tree.v (Span.text label) children in
  let tree = n "root" [ n "a" [ n "leaf label that is long" [] ]; n "b" [] ] in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 20;
  Tree.pp ppf tree;
  Format.pp_print_flush ppf ();
  str "wrapped tree"
    (String.concat "\n"
       [
         "root";
         "├── a";
         "│   └── leaf label";
         "│            that is";
         "│            long";
         "└── b";
       ])
    (Buffer.contents buf);
  str "to_string is natural"
    "root\n├── a\n│   └── leaf label that is long\n└── b" (Tree.to_string tree)

(* A styled guide is drawn in its style. *)
let test_guide_style () =
  let blue = Color.rgb 0 0 255 in
  let guide = Guide.with_style (Style.fg blue) Guide.unicode in
  let tree =
    Tree.v ~guide (Span.text "r")
      [ Tree.v (Span.text "a") []; Tree.v (Span.text "b") [] ]
  in
  Alcotest.(check string)
    "rows"
    "r\n\
     \027[38;2;0;0;255m\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 \027[0ma\n\
     \027[38;2;0;0;255m\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 \027[0mb"
    (Tree.to_string ~styled:true tree);
  Alcotest.(check bool)
    "with_style" true
    (Style.equal (Style.fg blue) (Guide.style guide));
  Alcotest.(check string)
    "plain"
    "r\n\
     \xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 a\n\
     \xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 b"
    (Tree.to_string tree)

let suite =
  ( "tree",
    [
      Alcotest.test_case "guide style" `Quick test_guide_style;
      Alcotest.test_case "prefix unicode" `Quick test_prefix_unicode;
      Alcotest.test_case "prefix ascii" `Quick test_prefix_ascii;
      Alcotest.test_case "render exact" `Quick test_render_exact;
      Alcotest.test_case "render single" `Quick test_render_single;
      Alcotest.test_case "margin wraps" `Quick test_margin_wraps;
    ] )
