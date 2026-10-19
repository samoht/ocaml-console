(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trees drawn with guide lines.

    A tree is a root span and its children, each drawn on its own row behind the
    {!Guide} glyphs that connect it to its parent. *)

(** {1:trees Trees} *)

(** The type for trees of ['a] nodes. *)
type 'a node = Node of 'a * 'a node list

type t
(** The type for renderable trees of spans. *)

val v : ?theme:Theme.t -> ?guide:Guide.t -> Span.t node -> t
(** [v tree] is [tree] ready to render. The branch glyphs come from [guide] if
    given, else from {!Theme.guide} of [theme], else {!Guide.unicode}.

    A tree of a recursive value is built by recursion over it:
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

    let rec node dir =
      Console.Tree.Node (Console.Span.text dir.name, List.map node dir.children)

    let tree = Console.Tree.v ~guide:Console.Guide.ascii (node root_dir)

    let () =
      assert (
        Console.Tree.to_string tree = "root\n+-- bin\n+-- etc\n    +-- hosts")
    ]} *)

(** {1:guides Guides} *)

val prefix : ?guide:Guide.t -> bool list -> string
(** [prefix ?guide lasts] is the guide prefix drawn before a node, where [lasts]
    is the is-last-among-siblings flag at each level from a top-level node down
    to and including this one. Each strict ancestor adds a vertical continuation
    ({!Guide.pipe}) or blank ({!Guide.space}) depending on whether it was the
    last of its siblings, then the node itself adds the {!Guide.branch} or
    {!Guide.last} connector. The empty list is the root and has no prefix. So
    under {!Guide.unicode} the first child of a non-last parent gives
    ["│   ├── "]. A live renderer that draws its own rows (a progress tree)
    shares the guide drawing through it. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a tree, one node per row behind its guide. The formatter's
    margin caps every row: a label wider than the room its guide leaves wraps at
    its words ({!Span.wrap}), each continuation hanging under the text after the
    label's first word ({!Span.hanging}) beside the guide of its level. *)

val to_string : ?styled:bool -> t -> string
(** [to_string tree] is [tree] rendered at its natural width, with ANSI styling
    iff [styled] (defaults to [false]). *)
