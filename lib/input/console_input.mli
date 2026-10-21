(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A line editor that does no input or output.

    An editor turns the bytes a user types into what to echo and the lines
    entered: it keeps the line being typed, inserts and erases at a cursor moved
    with the left and right arrows, recalls earlier lines with the up and down
    arrows and completes the current word on Tab. It reads and writes nothing
    itself, so a network channel, a serial console or a terminal in raw mode
    drives it alike: the program reads bytes, passes them to {!feed} and writes
    the echo back. To ask a question on the program's own terminal, use
    [Console_eio.Prompt], which reads the answer and uses an editor to hide a
    password.

    Editing is by grapheme cluster (UAX #29) at the cursor, a character with its
    marks: a printable character, even one whose bytes arrive across separate
    {!feed} calls, is inserted, joining the cluster before the cursor if it is a
    mark; backspace erases the cluster before the cursor, and the left and right
    arrows step over one cluster. A cluster takes the cells a terminal draws it
    in, two for a wide character or an emoji, so cursor motion is exact for
    them.

    The keys, the history and the completion follow those of the
    {{:https://erratique.ch/software/down}down} toplevel line editor. *)

type t
(** The type for line editors: the current line and the history of submitted
    ones. *)

val v :
  ?prompt:string ->
  ?history:string list ->
  ?complete:(string -> string list) ->
  ?mask:string ->
  unit ->
  t
(** [v ()] is a line editor with an empty line, such that:

    - [prompt] (defaults to [""]) is written again before the current line after
      Tab lists several completions, so that the line is redrawn behind its
      prompt. It stays empty when the caller writes no prompt;
    - [history] are the lines the up arrow recalls, oldest first (defaults to
      [[]]);
    - [complete word] is the list of completions of [word], the last word of the
      line, when Tab is pressed. The editor extends [word] to the longest prefix
      the completions share and lists them when more than one is left. Without
      [complete], Tab does nothing;
    - [mask], if given, is echoed once for each cluster typed, in place of the
      cluster, while {!pending} and the lines of {!feed} hold the bytes typed as
      they are, other than the keys: the replacement character, the space for a
      C1 control and the dropped mark of {!feed} are for an echo, and a byte
      above [0x7F] that is no character is kept. A masked editor, for a password
      or a token, completes nothing, recalls nothing and keeps no line in its
      {!history}.

    A control character in [prompt], [history], [mask] or a completion is echoed
    as a space, so none of them can move the cursor or change colours. *)

val feed : t -> string -> string * string list
(** [feed t input] is [(echo, lines)] once each byte of [input] is read in turn:
    [echo] is the bytes to write back to the terminal and [lines] the lines
    [input] completes, in order, each without its terminator. The bytes act as
    follows:

    - a printable character, ASCII \[[0x20];[0x7E]\] or a UTF-8 character of
      several bytes that may arrive across several [feed] calls, is inserted at
      the cursor and echoed; a mark of no width with nothing before it in the
      line to join is dropped, as a terminal drops it in the first column. A
      byte sequence that decodes to no character is the replacement character
      U+FFFD, and a C1 control (U+0080 to U+009F), which a terminal acts on, is
      a space, so what is echoed and kept is text that only prints;
    - [\127] (DEL) and [\008] (BS) erase the cluster before the cursor, echoing
      ["\b \b"] at the end of the line;
    - ['\r'] and ['\n'] submit the line, but a ['\n'] right after a ['\r'], the
      second half of a CRLF, submits nothing;
    - [\t] completes the current word ({!v});
    - [ESC \[ A] and [ESC \[ B] recall the previous and the next line of the
      history, and [ESC \[ C] and [ESC \[ D] move the cursor right and left over
      one cluster, as do [ESC O A] to [ESC O D], which a terminal in application
      cursor mode sends;
    - any other byte is ignored.

    Feeding [input] at once or in pieces gives the same echo and lines, and the
    echo holds no escape code: the cursor moves by backspaces. *)

val pending : t -> string
(** [pending t] is the current line, the bytes typed since the last submitted
    line. *)

val history : t -> string list
(** [history t] is the submitted lines, oldest first, after the [history] seed
    and with consecutive duplicates dropped. *)
