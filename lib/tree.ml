(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The guide type is shared with {!Theme}, so one theme styles a tree's nesting
   the same way it styles the live display's. *)
type guide = Theme.guide = {
  branch : string;
  last : string;
  pipe : string;
  space : string;
}

type 'a node = Node of 'a * 'a node list

(* Internal record; exposed as abstract [t] in the interface *)
type view = { guide : guide; tree : Span.t node }
type t = view

let ascii_guide =
  { branch = "+-- "; last = "+-- "; pipe = "|   "; space = "    " }

let unicode_guide =
  { branch = "├── "; last = "└── "; pipe = "│   "; space = "    " }

(* The guide a render uses: an explicit [guide] wins, else the [theme]'s guide,
   else the unicode default. *)
let resolve_guide ?theme ?guide () =
  match guide with
  | Some g -> g
  | None -> (
      match theme with Some t -> t.Theme.guide | None -> unicode_guide)

let of_tree ?theme ?guide tree =
  { guide = resolve_guide ?theme ?guide (); tree }

let v ?theme ?guide f root =
  let rec render_fn node = f render_fn node in
  { guide = resolve_guide ?theme ?guide (); tree = render_fn root }

(* The guide prefix for a node, given the is-last-among-siblings flag at each
   level from a top-level node down to and including this one. Each strict
   ancestor contributes a vertical continuation ([pipe]) or blank ([space])
   depending on whether it was the last of its siblings; the node itself
   contributes the [branch] or [last] connector. The empty list is the root,
   which has no prefix. *)
let prefix ?(guide = unicode_guide) lasts =
  match List.rev lasts with
  | [] -> ""
  | own :: rev_ancestors ->
      let buf = Buffer.create 16 in
      List.iter
        (fun ancestor_is_last ->
          Buffer.add_string buf
            (if ancestor_is_last then guide.space else guide.pipe))
        (List.rev rev_ancestors);
      Buffer.add_string buf (if own then guide.last else guide.branch);
      Buffer.contents buf

let pp ppf t =
  (* [lasts] is the is-last flag at each level from the top down to this node;
     the root carries the empty list and prints with no prefix. A literal
     newline separates nodes -- a [Fmt.cut] break hint collapses with no
     enclosing box, which is why the tree used to print on one line. *)
  let rec go lasts (Node (label, children)) =
    (match lasts with
    | [] -> ()
    | _ ->
        Fmt.string ppf "\n";
        Fmt.string ppf (prefix ~guide:t.guide lasts));
    Span.pp ppf label;
    let n = List.length children in
    List.iteri (fun i child -> go (lasts @ [ i = n - 1 ]) child) children
  in
  go [] t.tree

let to_string t =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let anim t = Anim.const (to_string t)
