(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { guide : Guide.t option; label : Span.t; children : t list }

let v ?guide label children = { guide; label; children }

(* A label wider than the margin leaves it wraps, the rest under its text. *)
let label_lines ~room label =
  List.concat_map
    (fun line ->
      if room <= 0 then [ Span.empty ]
      else if Span.width line <= room then [ line ]
      else Span_core.wrap_hanging room line)
    (Span_core.lines label)

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
    let room = margin - Span.width (Span.text first) in
    (match label_lines ~room node.label with
    | [] -> ()
    | l :: rest ->
        emit Span.(styled style first ++ l);
        List.iter (fun l -> emit Span.(styled style after ++ l)) rest);
    let n = List.length node.children in
    List.iteri (fun i child -> go ((i = n - 1) :: lasts) child) node.children
  in
  go [] t;
  List.rev !acc

let pp = Widget.pp lines
let to_string = Widget.to_string lines
