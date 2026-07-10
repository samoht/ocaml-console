(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains sub s = Re.execp Re.(compile (str sub)) s

(* The DOS preset is ASCII through and through: a blocky bar, the ASCII spinner
   and tree guide, a [+] done marker and an [OK]/[FAIL] result banner. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check bool) "blocky bar" true (t.bar = `Blocky);
  Alcotest.(check string) "done marker" "[+]" t.done_marker;
  Alcotest.(check string) "ok marker" "[OK]" t.ok_marker;
  Alcotest.(check string) "fail marker" "[FAIL]" t.fail_marker;
  Alcotest.(check int)
    "ascii spinner has four frames" 4
    (Array.length (Spinner.frames t.spinner));
  Alcotest.(check string) "ascii guide branch" "+-- " t.guide.branch

(* The unicode preset uses a smooth bar, the braille spinner and check-mark
   verdict markers. *)
let test_unicode () =
  let t = Theme.unicode in
  Alcotest.(check bool) "smooth bar" true (t.bar = `Smooth);
  Alcotest.(check string) "check-mark done" "✔" t.done_marker;
  Alcotest.(check string) "tick ok" "✔" t.ok_marker;
  Alcotest.(check string) "cross fail" "✘" t.fail_marker;
  Alcotest.(check int)
    "braille spinner has ten frames" 10
    (Array.length (Spinner.frames t.spinner))

(* The rainbow preset uses the gradient bar and cycles the spectrum. *)
let test_rainbow () =
  let t = Theme.rainbow in
  Alcotest.(check bool) "rainbow bar" true (t.bar = `Rainbow);
  Alcotest.(check int) "palette of six colours" 6 (List.length t.palette)

(* Only the matrix preset asks for an animated (rain) border. *)
let test_animated_border () =
  Alcotest.(check bool) "matrix animates" true Theme.matrix.animated_border;
  Alcotest.(check bool) "dos still" false Theme.dos.animated_border;
  Alcotest.(check bool) "unicode still" false Theme.unicode.animated_border;
  Alcotest.(check bool) "rainbow still" false Theme.rainbow.animated_border

(* [default] is the DOS preset. *)
let test_default () =
  Alcotest.(check bool)
    "default is dos" true
    (Theme.equal Theme.default Theme.dos)

(* [v] keeps the given fields and fills the rest with modern defaults: a cyan
   accent, a smooth bar, the unicode guide and [OK]/[FAIL] markers. *)
let test_v_defaults () =
  let t = Theme.v ~spinner:Spinner.ascii ~done_marker:"x" () in
  Alcotest.(check string) "given done marker" "x" t.done_marker;
  Alcotest.(check bool) "default cyan accent" true (t.accent = `Cyan);
  Alcotest.(check bool) "default smooth bar" true (t.bar = `Smooth);
  Alcotest.(check string) "default unicode guide" "├── " t.guide.branch;
  Alcotest.(check string) "default ok marker" "[OK]" t.ok_marker;
  Alcotest.(check string) "default fail marker" "[FAIL]" t.fail_marker

(* [with_verdict] replaces only the verdict markers, leaving the rest intact. *)
let test_with_verdict () =
  let t = Theme.with_verdict ~ok:"PASS" ~fail:"NOPE" Theme.dos in
  Alcotest.(check string) "ok replaced" "PASS" t.ok_marker;
  Alcotest.(check string) "fail replaced" "NOPE" t.fail_marker;
  Alcotest.(check string) "done marker untouched" "[+]" t.done_marker;
  (* an omitted field keeps the original marker *)
  let t2 = Theme.with_verdict ~ok:"Y" Theme.dos in
  Alcotest.(check string) "fail kept when omitted" "[FAIL]" t2.fail_marker

(* [pp] summarises the markers; the rendered text mentions each. *)
let test_pp () =
  let s = Fmt.str "%a" Theme.pp Theme.dos in
  Alcotest.(check bool) "mentions the done marker" true (contains "[+]" s);
  Alcotest.(check bool) "mentions the ok marker" true (contains "[OK]" s)

let suite =
  ( "theme",
    [
      Alcotest.test_case "dos preset" `Quick test_dos;
      Alcotest.test_case "unicode preset" `Quick test_unicode;
      Alcotest.test_case "rainbow preset" `Quick test_rainbow;
      Alcotest.test_case "animated border" `Quick test_animated_border;
      Alcotest.test_case "default" `Quick test_default;
      Alcotest.test_case "v defaults" `Quick test_v_defaults;
      Alcotest.test_case "with_verdict" `Quick test_with_verdict;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
