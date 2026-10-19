(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains sub s = Re.execp Re.(compile (str sub)) s

(* The DOS preset is ASCII through and through: a blocky bar, the ASCII spinner
   and tree guide. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check int)
    "ascii spinner has four frames" 4
    (Array.length (Spinner.frames (Theme.spinner t)));
  Alcotest.(check string)
    "ascii guide branch" "+-- "
    (Guide.branch (Theme.guide t))

let test_unicode () =
  let t = Theme.unicode in
  Alcotest.(check int)
    "braille spinner has ten frames" 10
    (Array.length (Spinner.frames (Theme.spinner t)))

let test_rainbow () =
  let t = Theme.rainbow in
  Alcotest.(check int)
    "palette of six colours" 6
    (List.length (Theme.palette t))

(* [v] is the unicode theme with the parts it is given replaced, and its
   palette follows its accent. *)
let test_v_defaults () =
  let t = Theme.v ~accent:Color.red ~spinner:Spinner.ascii () in
  Alcotest.(check string)
    "default unicode guide" "├── "
    (Guide.branch (Theme.guide t));
  Alcotest.(check bool)
    "palette is the accent" true
    (match Theme.palette t with [ c ] -> Color.equal c Color.red | _ -> false);
  Alcotest.(check int)
    "the spinner given" 4
    (Array.length (Spinner.frames (Theme.spinner t)))

let test_pp () =
  let s = Fmt.str "%a" Theme.pp Theme.dos in
  Alcotest.(check bool) "mentions the done marker" true (contains "[+]" s)

let suite =
  ( "theme",
    [
      Alcotest.test_case "dos preset" `Quick test_dos;
      Alcotest.test_case "unicode preset" `Quick test_unicode;
      Alcotest.test_case "rainbow preset" `Quick test_rainbow;
      Alcotest.test_case "v defaults" `Quick test_v_defaults;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
