(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { guide : Guide.t; label : Span.t; children : t list }

let v ?(guide = Guide.unicode) label children = { guide; label; children }

(* The guide prefix for a node, given the is-last-among-siblings flag at each
   level from a top-level node down to and including this one. Each strict
   ancestor contributes a vertical continuation ([pipe]) or blank ([space])
   depending on whether it was the last of its siblings; the node itself
   contributes the [branch] or [last] connector. The empty list is the root,
   which has no prefix. *)
let prefix ?(guide = Guide.unicode) lasts =
  match List.rev lasts with
  | [] -> ""
  | own :: rev_ancestors ->
      let buf = Buffer.create 16 in
      List.iter
        (fun ancestor_is_last ->
          Buffer.add_string buf
            (if ancestor_is_last then Guide.space guide else Guide.pipe guide))
        (List.rev rev_ancestors);
      Buffer.add_string buf
        (if own then Guide.last guide else Guide.branch guide);
      Buffer.contents buf

(* The guide beside the continuation of a node's label: each ancestor's, then
   the node's own vertical line if a sibling follows it. *)
let continuation ~guide lasts =
  String.concat ""
    (List.map
       (fun last -> if last then Guide.space guide else Guide.pipe guide)
       lasts)

let pp ppf t =
  let guide = t.guide in
  let margin = Format.pp_get_margin ppf () in
  let p = Paint.v ppf in
  let style = Guide.style guide in
  (* [lasts] is the is-last flag at each level from the top down to this node;
     the root carries the empty list and prints with no prefix. A literal
     newline separates nodes: a [Fmt.cut] break hint collapses with no
     enclosing box. *)
  let rec go lasts { label; children; _ } =
    let prefix = prefix ~guide lasts in
    if lasts <> [] then (
      Paint.newline p;
      Paint.ink p style prefix);
    let label = Span.sanitize ~keep_newlines:false label in
    let room = max 1 (margin - Width.string_width prefix) in
    (match
       if Span.width label <= room then [ label ]
       else Span.wrap_hanging room label
     with
    | [] -> ()
    | first :: rest ->
        Paint.close p;
        Span.pp ppf first;
        let beside = continuation ~guide lasts in
        List.iter
          (fun line ->
            Paint.newline p;
            Paint.ink p style beside;
            Paint.close p;
            Span.pp ppf line)
          rest);
    let n = List.length children in
    List.iteri (fun i child -> go (lasts @ [ i = n - 1 ]) child) children
  in
  go [] t;
  Paint.close p

let to_string = Render.to_string pp
