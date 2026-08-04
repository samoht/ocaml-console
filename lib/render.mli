(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Internal conversion of formatter output to strings. *)

val to_string : ?style_renderer:Fmt.style_renderer -> 'a Fmt.t -> 'a -> string

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] validates UTF-8, replaces malformed sequences with the Unicode
    replacement character, and replaces terminal control characters with a
    space. Newlines are retained unless [keep_newlines] is [false]. *)

val newline : Format.formatter -> unit
(** End one physical output row. ANSI formatters use CRLF so a row that exactly
    fills the terminal cannot leave the terminal in pending-wrap state. *)
