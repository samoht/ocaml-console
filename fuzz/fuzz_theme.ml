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
  (match Theme.of_string s with
  | Ok t -> check (String.equal (Theme.name t) s)
  | Error (`Msg _) -> check (not (List.mem_assoc s Theme.all)));
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

let themes =
  Theme.
    [
      default;
      ascii;
      dos;
      matrix;
      rainbow;
      dracula;
      nord;
      solarized_dark;
      solarized_light;
      gruvbox_dark;
      gruvbox_light;
      catppuccin_latte;
      catppuccin_mocha;
      tokyo_night;
      one_dark;
      monokai;
      github_dark;
      github_light;
    ]

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

(* On a formatter that is not UTF-8, every widget and a live display print
   ASCII alone, in every theme, for any ASCII text at any margin. *)
let test_ascii s n =
  let n = n land max_int in
  let s = String.map (fun c -> if Char.code c < 0x80 then c else '?') s in
  let text = Span.text s and margin = 1 + (n mod 60) in
  let theme = List.nth themes (n mod List.length themes) in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf false;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Format.pp_set_margin ppf margin;
  let leaf = Tree.v text [] in
  Fmt.pf ppf "%a@.%a@.%a@.%a@.%a@." Table.pp
    (Table.v
       [ Table.column s; Table.column ~overflow:`Truncate s ]
       [ [ text; text ]; [ text ] ])
    Panel.pp
    (Panel.v ~title:text ~subtitle:text text)
    Tree.pp
    (Tree.v text [ Tree.v text [ leaf ]; leaf ])
    Rule.pp (Rule.v ~label:text ()) Bar.pp
    (Bar.v ~brackets:true ~width:(n mod 30) (Float.of_int (n mod 101) /. 100.));
  let d =
    Display.v ~header:s
      ~size:(fun () -> (margin, 1 + (n mod 7)))
      ~now:(fun () -> 0.25)
      ppf
  in
  let parent = Display.task d s in
  let child = Display.task ~parent d s in
  Display.set_count child ~cur:(n mod 5) ~total:4;
  Display.log ~level:`Error child s;
  ignore (Display.task d s);
  Display.message ~level:`Warning d s;
  Display.succeed child;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  if not (is_ascii out) then failf "not ASCII: %S" out

let suite =
  ( "theme",
    [
      test_case "parts and marks" [ bytes; int ] test_theme;
      test_case "ASCII when not UTF-8" [ bytes; int ] test_ascii;
    ] )
