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
