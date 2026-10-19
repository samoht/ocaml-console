(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    Text from outside the program (a subprocess's output, a name read off the
    network, a file name) can hold bytes a terminal acts on: escape codes that
    move the cursor, clear the screen or change colours. These functions return
    text that only prints. This library has no other dependency on [console], so
    code that must stay small can use it alone. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] with each control character a space and each invalid
    UTF-8 byte the replacement character U+FFFD. Line feeds are kept unless
    [keep_newlines] is [false]. An escape code in [s] stays visible as text. *)

val strip : ?keep_newlines:bool -> string -> string
(** [strip s] is the text a terminal shows of [s]: {!sanitize} of [s] once each
    escape code is removed whole, a hyperlink's target or a window title
    included. *)

val sanitize_styles : ?keep_newlines:bool -> string -> string
(** [sanitize_styles s] is like {!strip} but keeps the escape codes that only
    set colours and text attributes, for relaying the output of a tool that
    coloured it. A colour left on at the end of [s] is turned off, so [s] cannot
    change the look of what is printed after it. *)

(**/**)

val fold_tokens :
  ('a -> [ `Text | `Escape ] -> int -> int -> 'a) -> 'a -> string -> 'a
(** [fold_tokens f acc s] folds [f] over the pieces of [s] left to right, each
    given by its kind, byte offset and length: an escape code, or a run of text.
    The pieces cover [s]. *)

val sgr : string -> [ `Set | `Reset ] option
(** [sgr e] is [Some `Reset] if the escape code [e] turns every text attribute
    off, [Some `Set] if it only sets colours or attributes, and [None]
    otherwise. *)

(**/**)
