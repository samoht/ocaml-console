(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Internal conversion of formatter output to strings. *)

val to_string : 'a Fmt.t -> ?styled:bool -> 'a -> string
(** [to_string pp ~styled v] is [pp v] on a formatter with no margin, so a
    widget renders at its natural width, with ANSI styling iff [styled]
    (defaults to [false]). *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] validates UTF-8, replaces malformed sequences with the Unicode
    replacement character, and replaces terminal control characters with a
    space. Newlines are retained unless [keep_newlines] is [false]. *)

val sanitize_styles : ?keep_newlines:bool -> string -> string
(** [sanitize_styles s] is {!sanitize} over [s] except that a strictly-parsed
    SGR sequence survives, and the result is closed with an SGR reset when the
    last one left a style open. *)

val style_is_open : string -> bool
(** [style_is_open s] is [true] iff the last SGR sequence in [s] leaves a style
    on, so that what follows [s] would be drawn in it. *)

val newline : Format.formatter -> unit
(** [newline ppf] ends one output row on [ppf]. An ANSI formatter gets CRLF, so
    that a row filling the terminal exactly cannot leave the terminal in its
    pending-wrap state. *)
