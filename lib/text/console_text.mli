(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    Text from outside the program can hold bytes that a terminal acts on, such
    as escape codes that move the cursor, clear the screen or change colours. A
    subprocess's output, a name read off the network or a file name are such
    text. These functions return text that only prints.

    This library is [console.text]. It does not need the rest of [console], so
    code that must stay small can use it alone. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] made safe to print, with an escape code in it kept
    visible as text.

    - Each control character (C0, DEL and C1) is a space.
    - Each invalid UTF-8 byte is the replacement character U+FFFD.
    - The characters that reorder or hide the text around them are removed. They
      are the bidirectional marks U+061C, U+200E and U+200F, the embeddings and
      overrides U+202A to U+202E, the isolates U+2066 to U+2069, and the
      interlinear annotation characters U+FFF9 to U+FFFB.
    - The joiners U+200C and U+200D, which some scripts and emoji need, are
      kept.
    - A line ends at a line feed, a carriage return, or the two together. VT, FF
      and NEL (U+0085), which a terminal reads as a line feed, end a line too.
      Each line end is one line feed, or one space if [keep_newlines] is [false]
      (defaults to [true]).

    The result is valid UTF-8, with no control character but line feeds, and
    [sanitize (sanitize s) = sanitize s]. *)

val strip : ?keep_newlines:bool -> ?keep_styles:bool -> string -> string
(** [strip s] is the text a terminal shows of [s]: the {!sanitize} of [s] once
    each escape code is removed whole.

    - A hyperlink's target and a window title go with their escape code.
    - [ESC E], the 7-bit next line, ends a line instead.
    - DEL and the C0 controls that a terminal acts on without printing are
      removed too, all but a tab, ESC and the line ends: [strip "a\007b"] is
      ["ab"].
    - Each line end is one line feed, or one space if [keep_newlines] is [false]
      (defaults to [true]), as with {!sanitize}.

    Without [keep_styles], the result holds no escape and
    [strip (strip s) = strip s].

    If [keep_styles] (defaults to [false]), the escape codes that only set
    colours and text attributes are kept, to relay the output of a tool that
    coloured it. A colour left on at the end of [s] is turned off, so [s] cannot
    change the look of what is printed after it. *)
