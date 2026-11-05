(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { label : View_core.t; children : t list }

let v label children = { label; children }
let label t = t.label
let children t = t.children

(* The first line of a label is laid out in the room beside the node's
   branch, its other lines in the room beside its rails. A line wider than
   its room wraps, hanging under its text. *)
let label_lines ~first ~rest label =
  List.concat
    (List.mapi
       (fun i line ->
         let room = if i = 0 then first else rest in
         if room <= 0 then [ View_core.empty ]
         else if View_core.width line <= room then [ line ]
         else View_core.wrap_hanging room line)
       (View_core.lines label))

let rec labels t = t.label :: List.concat_map labels t.children

let lines c ~guide ~indent t =
  let look = Ctx.look c in
  let guide = Widget.guide look guide in
  let style = Guide.style guide in
  let margin = Option.value (Ctx.width c) ~default:max_int in
  let pad = String.make indent ' ' in
  let acc = ref [] in
  (* A tree has few distinct guide strings, so each is measured once. *)
  let measured = Hashtbl.create 8 in
  let cells s =
    match Hashtbl.find_opt measured s with
    | Some n -> n
    | None ->
        let n = Widget.cells s in
        Hashtbl.add measured s n;
        n
  in
  (* A label line already fits its room, so a line is cut only when its
     guides alone are wider than the margin. *)
  let emit (prefix, cells) line =
    let line = View_core.(ink style prefix ++ line) in
    let line =
      if cells > margin then Widget.truncate look margin line else line
    in
    acc := line :: !acc
  in
  (* [lasts] says, for the node and each of its ancestors below the root,
     whether it is the last of its siblings, innermost first. *)
  let rec go lasts node =
    let lead = if lasts = [] then "" else pad in
    let first = lead ^ Widget.branch guide lasts
    and after = pad ^ Widget.rails guide lasts in
    let first_cells = cells first and after_cells = cells after in
    let room = margin - first_cells and rest = margin - after_cells in
    let label =
      View_core.resolve
        (Ctx.with_width (Option.map (fun _ -> room) (Ctx.width c)) c)
        node.label
    in
    (match label_lines ~first:room ~rest label with
    | [] -> ()
    | l :: ls ->
        emit (first, first_cells) l;
        List.iter (emit (after, after_cells)) ls);
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
