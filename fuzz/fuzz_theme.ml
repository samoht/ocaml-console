(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console
module Driver = Display_harness.Driver

let marks =
  [ `Running; `Succeeded; `Failed; `Cancelled; `Info; `Warning; `Error; `Debug ]

(* [v] keeps what it is given, its palette is never empty, and a mark is one
   line of text. *)
let test_theme s n =
  let c = Color.ansi_256 (abs n land 255) in
  let palette = if n land 1 = 0 then [] else [ c; Color.red ] in
  let t =
    Theme.v ~accent:c ~palette ~succeeded:c ~muted:c ~marks:(fun _ -> s) ()
  in
  check (Color.equal (Theme.muted t) c);
  check (not (Style.equal (Theme.header (Theme.v ())) Style.none));
  check (Theme.palette t <> []);
  check (Color.equal (Theme.accent t) c && Color.equal (Theme.succeeded t) c);
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
    (t :: List.map snd Theme.all)

let themes = List.map snd Theme.all
let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

(* On a formatter that is not UTF-8, every widget and a live display print
   ASCII alone, in every theme, for any ASCII text at any margin. *)
let test_ascii s n =
  let n = n land max_int in
  let s = String.map (fun c -> if Char.code c < 0x80 then c else '?') s in
  let text = View.text s and margin = 1 + (n mod 60) in
  let theme = List.nth themes (n mod List.length themes) in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf false;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Format.pp_set_margin ppf margin;
  let leaf = Tree.v text [] in
  Fmt.pf ppf "%a@.%a@.%a@.%a@.%a@." View.pp
    (Table.view
       [
         Table.column (View.text s);
         Table.column ~overflow:`Truncate (View.text s);
       ]
       [ [ text; text ]; [ text ] ])
    View.pp
    (Panel.view ~title:text ~subtitle:text text)
    View.pp
    (Tree.view (Tree.v text [ Tree.v text [ leaf ]; leaf ]))
    View.pp (Rule.view ~label:text ()) View.pp
    (Bar.view ~brackets:true ~width:(n mod 30)
       (Float.of_int (n mod 101) /. 100.));
  let d =
    Display.v ~summary:s
      ~size:(fun () -> (margin, 1 + (n mod 7)))
      ~now:(fun () -> 0.25)
      ppf
  in
  let parent = Display.task d s in
  let child = Display.task ~parent d s in
  Display.set_count child ~cur:(n mod 5) ~total:4;
  Display.log child s;
  ignore (Display.task d s);
  Driver.report d `Warning s;
  Driver.report d `Debug s;
  Display.fail child;
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
