(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    [Console.sanitize] and [Console.strip] are these functions; this library
    holds them alone, for a caller that must not link the rest of [console]. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] with each malformed UTF-8 sequence the replacement
    character and each C0 control, DEL and C1 control a space. Newlines are kept
    unless [keep_newlines] is [false]. *)

val strip : ?keep_newlines:bool -> string -> string
(** [strip s] is the text a terminal shows of [s]: {!sanitize} of [s] once each
    control sequence (ECMA-48 5.4), control string (ECMA-48 5.6) and escape
    sequence (ECMA-35) is removed whole, under the rules [Console.strip] gives.
*)
