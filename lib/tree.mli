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
        Tree.v ~guide:Guide.ascii (View.text "root")
          [
            Tree.v (View.text "bin") [];
            Tree.v (View.text "etc") [ Tree.v (View.text "hosts") [] ];
          ])

    let () =
      assert (
        Console.Tree.to_string tree = "root\n+-- bin\n+-- etc\n    +-- hosts")
    ]} *)

(** {1:trees Trees} *)

type t
(** The type for trees. *)

val v : ?guide:Guide.t -> View.t -> t list -> t
(** [v label children] is the tree of [label] above [children]. Its lines are
    those of [guide] if given, else the guide of the formatter's theme
    ({!Theme.get}). A tree drawn under another takes the lines of the one above
    it. *)

(** {1:converting Converting} *)

val to_span : ?width:int -> t -> View.t
(** [to_span ~width tree] is [tree] as a view, to put in a cell, a panel, the
    label of another tree or beside other text. When it is printed, it is laid
    out as {!pp} lays it out at a margin of [width] (defaults to its natural
    width), in the formatter's theme and [Fmt.utf_8].

    Each of its lines takes the cells it takes in {!Theme.default} in UTF-8.
    Where the formatter's look lays a line out wider or narrower, as a frame of
    glyphs two cells wide does, the line is cut or padded to those cells. *)

val to_string : ?styled:bool -> ?width:int -> t -> string
(** [to_string t] is [t] as {!pp} formats it at a margin of [width], or at its
    natural width without [width], with colours iff [styled] (defaults to
    [false]). Its lines are joined by line feeds, with none after the last. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a tree, with no line end after its last line. No line is wider
    than the formatter's margin. A label wider than the room the margin leaves
    it wraps, and the rest goes under its text. *)
