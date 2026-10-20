(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    Text from outside the program (a subprocess's output, a name read off the
    network, a file name) can hold bytes a terminal acts on: escape codes that
    move the cursor, clear the screen or change colours. These functions return
    text that only prints. This library is [console.text]; it does not need the
    rest of [console], so code that must stay small can use it alone. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] with each control character (C0, DEL and C1) a space,
    each invalid UTF-8 byte the replacement character U+FFFD, and without the
    characters that reorder or hide the text around them: the bidirectional
    marks U+061C, U+200E and U+200F, embeddings and overrides U+202A to U+202E
    and isolates U+2066 to U+2069, and the interlinear annotation characters
    U+FFF9 to U+FFFB. The joiners U+200C and U+200D, which some scripts and
    emoji need, are kept. A line ends at a line feed, a next line (NEL, U+0085),
    a carriage return, or a carriage return and a line feed or a next line
    together; each line end is one line feed, or one space if [keep_newlines] is
    [false] (defaults to [true]). An escape code in [s] stays visible as text.
    The result is valid UTF-8 without a control character but line feeds, and
    [sanitize (sanitize s) = sanitize s]. *)

val strip : ?keep_newlines:bool -> string -> string
(** [strip s] is the text a terminal shows of [s]: {!sanitize} of [s] once each
    escape code is removed whole, a hyperlink's target or a window title
    included, but for [ESC E], the 7-bit next line, which ends a line. It holds
    no escape, and [strip (strip s) = strip s]. *)

val sanitize_styles : ?keep_newlines:bool -> string -> string
(** [sanitize_styles s] is like {!strip} but keeps the escape codes that only
    set colours and text attributes, for relaying the output of a tool that
    coloured it. A colour left on at the end of [s] is turned off, so [s] cannot
    change the look of what is printed after it. *)
