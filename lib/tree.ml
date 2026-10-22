(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { guide : Guide.t option; label : View_core.t; children : t list }

let v ?guide label children = { guide; label; children }

(* A label wider than the margin leaves it wraps, the rest under its text. *)
let label_lines ~room label =
  List.concat_map
    (fun line ->
      if room <= 0 then [ View_core.empty ]
      else if View_core.width line <= room then [ line ]
      else View_core.wrap_hanging room line)
    (View_core.lines label)

let lines look ~margin t =
  let guide = Widget.guide look t.guide in
  let style = Guide.style guide in
  let acc = ref [] in
  let emit line = acc := Widget.truncate look margin line :: !acc in
  (* [lasts] are the node's is-last-of-its-siblings flag and its ancestors'
     below the root, innermost first. *)
  let rec go lasts node =
    let first = Widget.branch guide lasts
    and after = Widget.rails guide lasts in
    let room = margin - View_core.width (View_core.text first) in
    (match label_lines ~room node.label with
    | [] -> ()
    | l :: rest ->
        emit View_core.(ink style first ++ l);
        List.iter (fun l -> emit View_core.(ink style after ++ l)) rest);
    let n = List.length node.children in
    List.iteri (fun i child -> go ((i = n - 1) :: lasts) child) node.children
  in
  go [] t;
  List.rev !acc

let to_span = Widget.to_span lines
let pp = Widget.pp lines
let to_string = Widget.to_string lines
