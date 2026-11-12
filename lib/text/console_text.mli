(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    Text from outside the program, such as a subprocess's output, a name read
    off the network or a file name, can hold bytes that a terminal acts on:
    escape codes that move the cursor, clear the screen or change colours. These
    functions return text that only prints.

    This library is [console.text]. It needs only
    {{:https://erratique.ch/software/uucp}uucp}, not the rest of [console], so
    code that must stay small can use it alone. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] made safe to print. An escape code in [s] stays visible
    as text.

    - Each control character (C0, DEL and C1) becomes a space.
    - Each maximal subpart of an ill-formed UTF-8 sequence, as
      [String.get_utf_8_uchar] decodes it, becomes the replacement character
      U+FFFD. Such a subpart is a lone byte, or the start of a character that
      the next byte does not continue: ["\xf0\x9f"] becomes one U+FFFD.
    - Characters that a terminal draws as nothing are removed, because they can
      hide text or reorder the text around them. These are the characters with
      Unicode's Default_Ignorable_Code_Point property, such as the bidirectional
      controls, the zero-width space U+200B, the word joiner U+2060, the
      zero-width no-break space U+FEFF, the Hangul fillers and the tag
      characters U+E0000 to U+E007F. The interlinear annotation characters
      U+FFF9 to U+FFFB are removed too. The soft hyphen U+00AD has the property
      as well: a terminal may draw it as a hyphen, a space or nothing, so it is
      removed, and a word that it split reads whole.
    - The flags of England, Scotland and Wales keep their tags. Each such flag
      is the black flag U+1F3F4, then the tag characters of [gbeng], [gbsct] or
      [gbwls], then the cancel tag U+E007F. These are the emoji tag sequences of
      Unicode's emoji-sequences.txt. Any other run of tags is removed, so that
      no hidden text can follow a flag, and the black flag stays alone.
    - A variation selector (U+FE00 to U+FE0F, U+E0100 to U+E01EF, or a Mongolian
      one) is kept after a base character. It is removed when there is no base:
      after a space, a control, a line end, a joiner or another selector. At the
      start of [s] it is kept, because its base may be at the end of the text
      before [s]. So a run of selectors keeps only its first selector, and
      cannot hide text.
    - The joiners U+200C and U+200D, which some scripts and emoji need, are
      kept.
    - A line ends at a line feed, a carriage return, or the two together. VT, FF
      and NEL (U+0085), which a terminal reads as a line feed, end a line too.
      Each line end becomes one line feed, or one space if [keep_newlines] is
      [false] (defaults to [true]).

    The result is valid UTF-8, with no control character except line feeds, and
    [sanitize (sanitize s) = sanitize s]. *)

val strip : ?keep_newlines:bool -> ?keep_styles:bool -> string -> string
(** [strip s] is the text that a terminal shows for [s]. It removes each whole
    escape code from [s], then applies {!sanitize}.

    - A hyperlink's target and a window title are removed with their escape
      code.
    - [ESC E] (the 7-bit next line) ends a line instead of being removed.
    - DEL and the C0 controls that a terminal acts on without printing are also
      removed, except a tab, ESC and the line ends: [strip "a\007b"] is ["ab"].
    - Each line end becomes one line feed, or one space if [keep_newlines] is
      [false] (defaults to [true]), as with {!sanitize}.

    Without [keep_styles], the result holds no escape code and
    [strip (strip s) = strip s].

    If [keep_styles] is [true] (defaults to [false]), the escape codes that only
    set colours and text attributes are kept. Use it to relay the coloured
    output of a tool. A colour left on at the end of [s] is turned off, so [s]
    cannot change the look of what is printed after it. *)

val boundary : string -> int
(** [boundary s] is the offset at which to cut [s] when more text follows it.
    The cut is never inside a UTF-8 character or inside an escape code. It is:
    - the start of the unfinished escape code at the end of [s], if there is one
      and it does not fill all of [s];
    - otherwise, the start of the last character of [s] if it is not valid
      UTF-8, because the bytes after [s] may complete it;
    - otherwise, the length of [s].

    Cut text that arrives in pieces, such as the output of a child process, at
    this offset. Then no escape code is split between two pieces given to
    {!strip}. *)
