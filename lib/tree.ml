(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { label : View_core.t; children : t list }

let v label children = { label; children }
let label t = t.label
let children t = t.children

(* A label wider than the room its guides leave it wraps, the rest under its
   text. *)
let label_lines ~room label =
  List.concat_map
    (fun line ->
      if room <= 0 then [ View_core.empty ]
      else if View_core.width line <= room then [ line ]
      else View_core.wrap_hanging room line)
    (View_core.lines label)

let rec labels t = t.label :: List.concat_map labels t.children

let lines c ~guide ~indent t =
  let look = Ctx.look c in
  let guide = Widget.guide look guide in
  let style = Guide.style guide in
  let margin = Option.value (Ctx.width c) ~default:max_int in
  let pad = String.make indent ' ' in
  let acc = ref [] in
  let emit line = acc := Widget.truncate look margin line :: !acc in
  (* [lasts] are the node's is-last-of-its-siblings flag and its ancestors'
     below the root, innermost first. *)
  let rec go lasts node =
    let lead = if lasts = [] then "" else pad in
    let first = lead ^ Widget.branch guide lasts
    and after = pad ^ Widget.rails guide lasts in
    let room = margin - Widget.cells first in
    let label =
      View_core.resolve
        (Ctx.with_width (Option.map (fun _ -> room) (Ctx.width c)) c)
        node.label
    in
    (match label_lines ~room label with
    | [] -> ()
    | l :: rest ->
        emit View_core.(ink style first ++ l);
        List.iter (fun l -> emit View_core.(ink style after ++ l)) rest);
    let n = List.length node.children in
    List.iteri (fun i child -> go ((i = n - 1) :: lasts) child) node.children
  in
  go [] t;
  List.rev !acc

let view ?guide ?(indent = 0) t =
  if indent < 0 then invalid_arg "Console.Tree.view: negative indent";
  let parts = labels t in
  View_core.deferred ~kind:(View_core.kinds parts) (fun c ->
      View_core.concat ~sep:(View_core.text "\n") (lines c ~guide ~indent t))
