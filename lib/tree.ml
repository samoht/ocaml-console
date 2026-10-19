(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { guide : Guide.t; label : Span.t; children : t list }

let v ?(guide = Guide.unicode) label children = { guide; label; children }

(* [lasts] are the is-last-of-its-siblings flags of a node's ancestors below
   the root, innermost first; [own] is the node's. *)
let node_prefix ~guide lasts own =
  String.concat ""
    (List.rev_map
       (fun last -> if last then Guide.space guide else Guide.pipe guide)
       lasts
    @ [ (if own then Guide.last guide else Guide.branch guide) ])

let prefix ?(guide = Guide.unicode) lasts =
  match List.rev lasts with
  | [] -> ""
  | own :: ancestors -> node_prefix ~guide ancestors own

let beside ~guide lasts =
  String.concat ""
    (List.rev_map
       (fun last -> if last then Guide.space guide else Guide.pipe guide)
       lasts)

(* A label wider than the margin leaves it wraps, the rest under its text. *)
let label_lines ~room label =
  List.concat_map
    (fun line ->
      if room <= 0 then [ Span.empty ]
      else if Span.width line <= room then [ line ]
      else Span.wrap_hanging room line)
    (Span.lines label)

let lines ~margin t =
  let guide = t.guide in
  let style = Guide.style guide in
  let acc = ref [] in
  let emit line = acc := Span.truncate margin line :: !acc in
  let rec go ~root lasts node =
    let first, after =
      if root then ("", "")
      else
        match lasts with
        | own :: ancestors ->
            (node_prefix ~guide ancestors own, beside ~guide lasts)
        | [] -> ("", "")
    in
    let room = margin - Span.width (Span.text first) in
    (match label_lines ~room node.label with
    | [] -> ()
    | l :: rest ->
        emit Span.(styled style first ++ l);
        List.iter (fun l -> emit Span.(styled style after ++ l)) rest);
    let n = List.length node.children in
    List.iteri
      (fun i child -> go ~root:false ((i = n - 1) :: lasts) child)
      node.children
  in
  go ~root:true [] t;
  List.rev !acc

let pp = Widget.pp lines
let to_string = Widget.to_string lines
