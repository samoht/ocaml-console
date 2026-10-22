(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Two builds side by side: a table of their status above, the tree of each
   build's steps in a column of its own, and the summary below, all put
   together with View.vcat and View.hcat. With [~keep_ended:true] the steps that
   end stay in their column, and the last frame stays once the builds are
   done. On a terminal too narrow for two columns, the trees go one under the
   other. Pipe it through [cat] to see the lines a CI log gets. *)

open Console

type build = { name : string; steps : (string * int) list; pace : float }

let builds =
  [
    {
      name = "linux";
      steps = [ ("fetch", 4); ("compile", 12); ("test", 8) ];
      pace = 0.08;
    };
    {
      name = "macos";
      steps = [ ("fetch", 4); ("compile", 12); ("test", 8); ("sign", 2) ];
      pace = 0.11;
    };
  ]

let status = Hashtbl.create 2
let set_status b s = Hashtbl.replace status b.name s

let table () =
  Table.view
    [
      Table.column (View.text "Build");
      Table.column ~min_width:7 (View.text "Status");
    ]
    (List.map
       (fun b ->
         [
           View.text b.name;
           Option.value
             (Hashtbl.find_opt status b.name)
             ~default:(View.text "waiting");
         ])
       builds)

(* The table, the trees side by side if each has 30 columns, and the
   summary. The display draws the last lines that fit, so the table gives
   way first. *)
let view display ~width ~height =
  let room = min width 100 in
  let half = (room - 2) / 2 in
  let height = height - 1 in
  let trees =
    match Display.(children (root display)) with
    | [ a; b ] when half >= 30 ->
        View.hcat
          [
            Display.tree ~width:half ~height a;
            Display.tree ~width:half ~height b;
          ]
    | _ -> Display.tree ~height (Display.root display)
  in
  View.vcat
    [ View.sized ~width:room (table ()); trees; Display.summary display ]

let run clock display b =
  set_status b (View.role `Accent (View.text "running"));
  Display.with_task display b.name (fun _ ->
      List.iter
        (fun (step, total) ->
          Display.with_task display step (fun task ->
              for cur = 1 to total do
                Eio.Time.Mono.sleep clock b.pace;
                Display.set_count task ~cur ~total
              done))
        b.steps);
  set_status b (View.role `Succeeded (View.text "done"))

let () =
  Console_tty.setup ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~summary:"Building" ~keep_ended:true ~view
    (fun display -> Eio.Fiber.List.iter (run clock display) builds)
