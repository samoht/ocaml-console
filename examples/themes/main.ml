(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The same table, tree and progress rows in the theme its argument names:
   the README's gallery is screenshots of this program, one per theme. *)

open Console

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
      let ok = Display.task d "fetch" in
      Display.set_count ok ~cur:3 ~total:3;
      Display.succeed ok;
      let failed = Display.task d "lint" in
      Display.fail failed;
      let compile = Display.task ~log_lines:1 d "compile" in
      Display.set_count compile ~cur:5 ~total:8;
      Display.log ~level:`Warning compile "unused variable x";
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
      Console_eio.setup ~style_renderer:`Ansi_tty ();
      Theme.set Fmt.stdout theme;
      widgets theme;
      Eio_main.run @@ fun env -> progress ~clock:(Eio.Stdenv.clock env)
