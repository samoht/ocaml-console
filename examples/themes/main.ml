(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The same table, tree and progress rows in the theme its argument names:
   the README's gallery is screenshots of this program, one per theme. *)

open Console

let themes =
  [
    ("unicode", Theme.default);
    ("dos", Theme.dos);
    ("matrix", Theme.matrix);
    ("rainbow", Theme.rainbow);
    ("dracula", Theme.dracula);
    ("nord", Theme.nord);
    ("solarized-dark", Theme.solarized_dark);
    ("solarized-light", Theme.solarized_light);
    ("gruvbox-dark", Theme.gruvbox_dark);
    ("gruvbox-light", Theme.gruvbox_light);
    ("catppuccin-latte", Theme.catppuccin_latte);
    ("catppuccin-mocha", Theme.catppuccin_mocha);
    ("tokyo-night", Theme.tokyo_night);
    ("one-dark", Theme.one_dark);
    ("monokai", Theme.monokai);
    ("github-dark", Theme.github_dark);
    ("github-light", Theme.github_light);
  ]

let widgets theme =
  let accent = Style.(bold + fg (Theme.accent theme)) in
  let table =
    Table.v
      [ Table.column "Package"; Table.column ~align:`Right "Size" ]
      [
        [ Span.styled accent "console"; Span.text "42 KiB" ];
        [ Span.text "console-eio"; Span.text "9 KiB" ];
      ]
  in
  let leaf s = Tree.v (Span.text s) [] in
  let tree =
    Tree.v (Span.styled accent "lib")
      [
        Tree.v (Span.text "display") [ leaf "row.ml"; leaf "region.ml" ];
        leaf "span.ml";
      ]
  in
  Fmt.pr "%a@.%a@.@." Table.pp table Tree.pp tree

let progress ~clock =
  Console_eio.run ~clock ~ppf:Fmt.stdout ~header:"Building" (fun d ->
      let ok = Console.Display.task d "fetch" in
      Console.Display.set_count ok ~cur:3 ~total:3;
      Console.Display.succeed ok;
      let failed = Console.Display.task d "lint" in
      Console.Display.fail failed;
      let compile = Console.Display.task ~log_lines:1 d "compile" in
      Console.Display.set_count compile ~cur:5 ~total:8;
      Console.Display.log ~level:`Warning compile "unused variable x";
      ignore (Console.Display.task d "test");
      Eio.Time.sleep clock 3.;
      Console.Display.cancel compile)

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "unicode" in
  let theme = try List.assoc name themes with Not_found -> Theme.default in
  Console_eio.setup ~style_renderer:`Ansi_tty ();
  Theme.set Fmt.stdout theme;
  widgets theme;
  Eio_main.run @@ fun env -> progress ~clock:(Eio.Stdenv.clock env)
