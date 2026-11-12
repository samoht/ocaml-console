(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The look of a formatter: the theme, the character set and the liveness that
    it draws with. *)

type t = { theme : Theme.t; utf_8 : bool; live : bool }
(** The type for how a formatter draws: its theme, whether it shows UTF-8 or
    only ASCII, and whether a live display draws in colour. The frames of the
    theme move only when [live] is [true]. *)

val v : ?live:bool -> Format.formatter -> t
(** [v ppf] is the look of [ppf]: its theme ({!Theme.get}), its {!Fmt.utf_8}
    setting, and [live] (defaults to [false]). *)

val default : t
(** [default] is the look of a formatter with no settings: {!Theme.default}, in
    UTF-8, not live. *)

val program : unit -> t
(** [program ()] is {!default} with the program's theme ({!Theme.program}). *)

val ascii_theme : t -> bool
(** [ascii_theme l] is [true] iff the theme of [l] draws in ASCII on any
    formatter: its bars are [`Ascii], or the lines of its frames are ASCII. *)

val ellipsis : t -> string
(** [ellipsis l] is the string that ends a line cut in [l]: U+2026 in UTF-8, or
    three dots in ASCII and in an {!ascii_theme}. *)

val equal : t -> t -> bool
(** [equal l0 l1] is [true] iff [l0] and [l1] have physically the same theme,
    and the same {!field-utf_8} and {!field-live}. *)

val pp : t Fmt.t
(** [pp] formats a look for diagnostics, in an unspecified format. *)
