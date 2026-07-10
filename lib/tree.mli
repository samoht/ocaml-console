(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tree rendering with customizable guides.

    Renders tree structures with ASCII or Unicode guide lines. *)

(** {1 Types} *)

type guide = Theme.guide = {
  branch : string;  (** Branch connector, e.g., ["├── "]. *)
  last : string;  (** Last-child connector, e.g., ["└── "]. *)
  pipe : string;  (** Vertical continuation, e.g., ["│   "]. *)
  space : string;  (** Blank continuation, e.g., ["    "]. *)
}
(** The guide glyphs that draw a tree's branches. Shared with {!Theme.guide}, so
    a {!Theme.t} styles a tree's branches and the live display's nesting
    identically. *)

type 'a node = Node of 'a * 'a node list  (** A generic tree node. *)

type t
(** A renderable tree of spans. *)

(** {1 Predefined Guides} *)

val ascii_guide : guide
(** [ascii_guide] is the ASCII guide: [+-- ], [+-- ], [|   ], [    ]. *)

val unicode_guide : guide
(** [unicode_guide] is the Unicode guide: [├── ], [└── ], [│   ], [    ]. *)

(** {1 Construction} *)

val of_tree : ?theme:Theme.t -> ?guide:guide -> Span.t node -> t
(** [of_tree tree] is a renderable tree from a tree of spans. The branch glyphs
    come from the guide argument if given, else from [theme]'s {!type-guide},
    else {!unicode_guide}. *)

val v :
  ?theme:Theme.t ->
  ?guide:guide ->
  (('a -> Span.t node) -> 'a -> Span.t node) ->
  'a ->
  t
(** [v f root] is a tree built by recursively applying [f]; the branch glyphs
    are resolved as in {!of_tree}.

    The function [f] receives a render function and the current node, and should
    return a tree of spans.

    Example:
    {[
    type dir = { name : string; children : dir list }

    let root_dir =
      {
        name = "root";
        children =
          [
            { name = "bin"; children = [] };
            { name = "etc"; children = [ { name = "hosts"; children = [] } ] };
          ];
      }

    let tree =
      Console.Tree.v
        (fun render (dir : dir) ->
          Console.Tree.Node
            (Console.Span.text dir.name, List.map render dir.children))
        root_dir

    let () = assert (String.length (Console.Tree.to_string tree) > 0)
    ]} *)

(** {1 Guides} *)

val prefix : ?guide:guide -> bool list -> string
(** [prefix ?guide lasts] is the guide prefix drawn before a node, where [lasts]
    is the is-last-among-siblings flag at each level from a top-level node down
    to and including this one. Each strict ancestor adds a vertical continuation
    ({!field-pipe}) or blank ({!field-space}) depending on whether it was the
    last of its siblings, then the node itself adds the {!field-branch} or
    {!field-last} connector. The empty list is the root and has no prefix. So
    under {!unicode_guide} the first child of a non-last parent gives
    ["│   ├── "]. A live renderer that draws its own rows (a progress tree)
    shares the guide drawing through it. *)

(** {1 Rendering} *)

val pp : t Fmt.t
(** [pp] pretty-prints the tree, one node per line, with guide lines. *)

val to_string : t -> string
(** [to_string tree] renders to a string. *)

(** {1 Animation} *)

val anim : t -> string Anim.t
(** [anim tree] is [tree] as a still {!Anim.t}: every frame is {!to_string}
    [tree]. A tree has no border, so there is no rain variant. *)
