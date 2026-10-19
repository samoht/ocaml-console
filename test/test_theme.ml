(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains sub s = Re.execp Re.(compile (str sub)) s

(* The DOS preset is ASCII through and through: a blocky bar, the ASCII spinner
   and tree guide. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check string)
    "ascii spinner" "/"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1);
  Alcotest.(check string)
    "ascii guide branch" "+-- "
    (Guide.branch (Theme.guide t))

let test_unicode () =
  let t = Theme.unicode in
  Alcotest.(check string)
    "braille spinner" "⠙"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1)

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
  Alcotest.(check string)
    "the spinner given" "/"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1)

(* Each scheme keeps the background its authors chose. *)
let test_schemes () =
  List.iter
    (fun (name, t, bg) ->
      Alcotest.(check (option string))
        name (Some bg)
        (Option.map (Fmt.str "%a" Color.pp) (Theme.background t)))
    [
      ("dracula", Theme.dracula, "#282a36");
      ("nord", Theme.nord, "#2e3440");
      ("solarized-dark", Theme.solarized_dark, "#002b36");
      ("solarized-light", Theme.solarized_light, "#fdf6e3");
      ("gruvbox-dark", Theme.gruvbox_dark, "#282828");
      ("gruvbox-light", Theme.gruvbox_light, "#fbf1c7");
      ("catppuccin-latte", Theme.catppuccin_latte, "#eff1f5");
      ("catppuccin-mocha", Theme.catppuccin_mocha, "#1e1e2e");
      ("tokyo-night", Theme.tokyo_night, "#1a1b26");
      ("one-dark", Theme.one_dark, "#282c34");
      ("monokai", Theme.monokai, "#272822");
      ("github-dark", Theme.github_dark, "#0d1117");
      ("github-light", Theme.github_light, "#ffffff");
    ]

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
      Alcotest.test_case "schemes" `Quick test_schemes;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
