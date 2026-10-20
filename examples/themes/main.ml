(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The same title, tree, table and progress rows in the theme its argument
   names, the title graded through the theme's palette: the README's
   gallery is screenshots of this program, one per theme. The
   table is drawn above the rows while they run, so a theme with motion
   shows its frame moving, and is printed once they are done. *)

open Console

let table theme =
  let accent = Style.(bold + fg (Theme.accent theme)) in
  Table.v ~rules:true
    [ Table.column "Package"; Table.column ~align:`Right "Size" ]
    [
      [ Span.styled accent "console"; Span.text "42 KiB" ];
      [ Span.text "console-eio"; Span.text "9 KiB" ];
    ]

let tree theme =
  let accent = Style.(bold + fg (Theme.accent theme)) in
  let leaf s = Tree.v (Span.text s) [] in
  Tree.v (Span.styled accent "lib")
    [
      Tree.v (Span.text "display") [ leaf "row.ml"; leaf "region.ml" ];
      leaf "span.ml";
    ]

let progress ~clock table =
  Console_eio.run ~clock ~ppf:Fmt.stdout ~header:"Building" (fun d ->
      Display.set_scene d (fun ~height:_ ppf -> Table.pp ppf table);
      let ok = Display.task d "fetch" in
      Display.set_count ok ~cur:3 ~total:3;
      Display.succeed ok;
      let failed = Display.task d "lint" in
      Display.fail failed;
      let compile = Display.task ~log_lines:1 d "compile" in
      Display.set_count compile ~cur:5 ~total:8;
      Display.log compile "unused variable x";
      ignore (Display.task d "test");
      Eio.Time.sleep clock 3.;
      Display.cancel compile)

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  match Theme.of_string name with
  | Error (`Msg e) ->
      prerr_endline e;
      exit 2
  | Ok theme ->
      Console_tty.setup ~style_renderer:`Ansi_tty ();
      Theme.set Fmt.stdout theme;
      let ocaml = String.map (function '-' -> '_' | c -> c) name in
      let title = Span.gradient ("Theme." ^ ocaml) in
      Fmt.pr "%a@.%a@.@." Span.pp
        (Span.with_style Style.bold title)
        Tree.pp (tree theme);
      let table = table theme in
      (Eio_main.run @@ fun env -> progress ~clock:(Eio.Stdenv.clock env) table);
      Fmt.pr "@.%a@." Table.pp table
