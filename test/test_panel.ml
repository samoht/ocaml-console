(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains pattern str = Re.execp (Re.compile (Re.str pattern)) str

let test_basic () =
  let panel = Panel.v (Span.text "content") in
  let output = Panel.to_string panel in
  Alcotest.(check bool) "contains content" true (contains "content" output)

let test_with_title () =
  let panel = Panel.v ~title:(Span.text "Title") (Span.text "body") in
  let output = Panel.to_string panel in
  Alcotest.(check bool) "contains title" true (contains "Title" output);
  Alcotest.(check bool) "contains body" true (contains "body" output)

(* A still panel animation is just its rendered string at every frame. *)
let test_anim_const () =
  let p = Panel.v (Span.text "body") in
  let a = Panel.anim p in
  Alcotest.(check string)
    "frame 0 is to_string" (Panel.to_string p) (Anim.frame a ~elapsed:0.);
  Alcotest.(check string)
    "frame 9 is to_string too" (Panel.to_string p) (Anim.frame a ~elapsed:9.)

let lines s = String.split_on_char '\n' s

(* The rain frame keeps the box rectangle (content height + 2 border rows), is
   coloured green, shows the content, and is deterministic in elapsed. *)
let test_rain () =
  let p = Panel.lines [ Span.text "neo"; Span.text "trinity" ] in
  let a = Panel.rain p in
  let f1 = Anim.frame a ~elapsed:0.4 in
  Alcotest.(check int)
    "content height + 2 border rows" 4
    (List.length (lines f1));
  Alcotest.(check bool) "green frame" true (contains "\027[32m" f1);
  Alcotest.(check bool) "content kept" true (contains "trinity" f1);
  (* deterministic: the same instant renders byte-identically *)
  Alcotest.(check string)
    "stable at a given elapsed" f1
    (Anim.frame a ~elapsed:0.4);
  (* the rain advances: a later frame differs *)
  Alcotest.(check bool)
    "frame advances over time" true
    (f1 <> Anim.frame a ~elapsed:1.7)

(* anim honours the theme: a matrix-themed panel rains, others stay still. *)
let test_anim_theme () =
  let m = Panel.v ~theme:Theme.matrix (Span.text "neo") in
  Alcotest.(check bool)
    "matrix theme rains" true
    (contains "\027[32m" (Anim.frame (Panel.anim m) ~elapsed:0.4));
  let u = Panel.v ~theme:Theme.unicode (Span.text "neo") in
  Alcotest.(check string)
    "non-matrix theme stays still" (Panel.to_string u)
    (Anim.frame (Panel.anim u) ~elapsed:0.4)

let suite =
  ( "panel",
    [
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "with title" `Quick test_with_title;
      Alcotest.test_case "anim is still" `Quick test_anim_const;
      Alcotest.test_case "matrix rain" `Quick test_rain;
      Alcotest.test_case "anim honours theme" `Quick test_anim_theme;
    ] )
