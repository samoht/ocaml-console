(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The same title, tree, table and progress rows in the theme its argument
   names, the title graded through the theme's palette: the README's
   gallery is screenshots of this program, one per theme. The table is
   drawn above the rows while they run, so a theme with motion shows its
   frame moving, and stays, still, in the view's last frame once they are
   done. *)

open Console

let accent s = View.styled Style.bold (View.role `Accent (View.text s))

let table =
  Table.view ~rules:true
    [
      Table.column (View.text "Package");
      Table.column ~align:`Right (View.text "Size");
    ]
    [
      [ accent "console"; View.text "42 KiB" ];
      [ View.text "console-eio"; View.text "9 KiB" ];
    ]

let tree =
  let leaf s = Tree.v (View.text s) [] in
  Tree.v (accent "lib")
    [
      Tree.v (View.text "display") [ leaf "row.ml"; leaf "region.ml" ];
      leaf "span.ml";
    ]

let progress ~clock table =
  (* The table above the rows and the summary. *)
  let view d ~width ~height =
    View.vcat
      [
        View.sized ~width:(min width 100) table;
        Display.default_view d ~width ~height;
      ]
  in
  Console_eio.run ~clock ~ppf:Fmt.stdout ~summary:"Building" ~view (fun d ->
      let ok = Display.task d "fetch" in
      Display.set_count ok ~cur:3 ~total:3;
      Display.succeed ok;
      let failed = Display.task d "lint" in
      Display.fail failed;
      let compile = Display.task ~log_lines:1 d "compile" in
      Display.set_count compile ~cur:5 ~total:8;
      Display.log compile "unused variable x";
      ignore (Display.task d "test");
      Eio.Time.Mono.sleep clock 3.;
      Display.cancel compile)

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  match Theme.of_string name with
  | Error (`Msg e) ->
      prerr_endline e;
      exit 2
  | Ok theme ->
      let no_color =
        match Sys.getenv_opt "NO_COLOR" with
        | Some "" | None -> false
        | _ -> true
      in
      Console_tty.setup
        ?style_renderer:(if no_color then None else Some `Ansi_tty)
        ~theme ();
      let ocaml = String.map (function '-' -> '_' | c -> c) name in
      let title = View.gradient (View.text ("Theme." ^ ocaml)) in
      Fmt.pr "%a@.%a@.@." View.pp
        (View.styled Style.bold title)
        View.pp (Tree.view tree);
      Eio_main.run @@ fun env ->
      progress ~clock:(Console_eio.clock (Eio.Stdenv.mono_clock env)) table
