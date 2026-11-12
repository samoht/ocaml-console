(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Two builds side by side, put together with View.vcat and View.hcat: a
   status table on top, the tree of each build's steps in its own column,
   and the summary below. With [~keep_ended:true], ended steps stay in their
   column, and the last frame stays when the builds are done. If the
   terminal is too narrow for two columns, the trees go one under the
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

(* The table, the two trees and the summary, as they are at each frame. The
   trees go side by side when each gets 30 columns. The display draws the
   last lines that fit, so the table gives way first. The trees take the
   height that the summary leaves, and the last frame shows them whole. *)
let view root =
  View.fit (fun r ->
      let room = min 100 (Option.value (Room.width r) ~default:100) in
      let half = (room - 2) / 2 in
      let height = Option.map (fun h -> h - 1) (Room.height r) in
      let trees =
        match Task.children root with
        | [ a; b ] when half >= 30 ->
            View.hcat
              [
                View.sized ~width:half ?height (Progress.tree a);
                View.sized ~width:half ?height (Progress.tree b);
              ]
        | _ -> View.sized ?height (Progress.tree root)
      in
      View.vcat
        [ View.sized ~width:room (table ()); trees; Progress.summary root ])

let run clock root b =
  set_status b (View.role `Accent (View.text "running"));
  Task.run root b.name (fun build ->
      List.iter
        (fun (step, total) ->
          Task.run build step (fun task ->
              for count = 1 to total do
                Eio.Time.Mono.sleep clock b.pace;
                Task.set_items task ~count ~total
              done))
        b.steps);
  set_status b (View.role `Succeeded (View.text "done"))

let () =
  Console_tty.setup ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~label:"Building" ~keep_ended:true ~view (fun root ->
      Eio.Fiber.List.iter (run clock root) builds)
