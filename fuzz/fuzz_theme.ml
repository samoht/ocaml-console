(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let marks =
  [ `Running; `Succeeded; `Failed; `Cancelled; `Info; `Warning; `Error ]

(* [v] keeps what it is given, its palette is never empty, and a mark is one
   line of text. *)
let test_theme s n =
  let c = Color.palette (abs n land 255) in
  let palette = if n land 1 = 0 then [] else [ c; Color.red ] in
  let t = Theme.v ~accent:c ~palette ~ok:c ~marks:(fun _ -> s) () in
  check (Theme.palette t <> []);
  check (Color.equal (Theme.accent t) c && Color.equal (Theme.ok t) c);
  List.iter
    (fun t ->
      check (Theme.palette t <> []);
      List.iter
        (fun m ->
          let m = Theme.mark t m in
          check (not (String.exists (fun c -> c = '\n' || c = '\027') m)))
        marks;
      Fmt.kstr ignore "%a" Theme.pp t)
    [
      t;
      Theme.default;
      Theme.dos;
      Theme.matrix;
      Theme.rainbow;
      Theme.dracula;
      Theme.nord;
      Theme.solarized_dark;
      Theme.solarized_light;
      Theme.gruvbox_dark;
      Theme.gruvbox_light;
      Theme.catppuccin_latte;
      Theme.catppuccin_mocha;
      Theme.tokyo_night;
      Theme.one_dark;
      Theme.monokai;
      Theme.github_dark;
      Theme.github_light;
    ]

let suite = ("theme", [ test_case "parts and marks" [ bytes; int ] test_theme ])
