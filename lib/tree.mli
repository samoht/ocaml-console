(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trees, drawn like the output of [tree].

    Each node is on its own row, behind the {!Guide} lines that connect it to
    its parent.

    {[
    let tree =
      Console.(
        Tree.v ~guide:Guide.ascii (Span.text "root")
          [
            Tree.v (Span.text "bin") [];
            Tree.v (Span.text "etc") [ Tree.v (Span.text "hosts") [] ];
          ])

    let () =
      assert (
        Console.Tree.to_string tree = "root\n+-- bin\n+-- etc\n    +-- hosts")
    ]} *)

type t
(** The type for trees. *)

val v : ?guide:Guide.t -> Span.t -> t list -> t
(** [v label children] is the tree of [label] above [children]. Its lines are
    those of [guide] (defaults to {!Guide.unicode}); a tree drawn under another
    takes the lines of the one above it. *)

val pp : t Fmt.t
(** [pp] formats a tree, with no line end after its last line. No line is wider
    than the formatter's margin: a label wider than the room the margin leaves
    it wraps, the rest under its text. *)

val to_string : ?styled:bool -> t -> string
(** [to_string t] is [t] formatted at its natural width, with colours iff
    [styled] (defaults to [false]). *)
