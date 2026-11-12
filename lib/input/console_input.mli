(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A line editor that does no input or output.

    An editor turns the bytes that a user types into the text to echo and the
    lines entered. It keeps the line being typed, and it:
    - inserts and erases at a cursor that the left and right arrows move;
    - recalls earlier lines with the up and down arrows;
    - completes the current word on Tab.

    It reads and writes nothing itself, so a network channel, a serial console
    or a terminal in raw mode can all drive it. The program reads bytes, passes
    them to {!feed}, and writes the echo back. To ask a question on the
    program's own terminal, use [Console_eio.Prompt]. It reads the answer, and
    uses an editor to hide a password.

    Editing works on grapheme clusters (UAX #29). A cluster is a character with
    its marks:
    - A printable character is inserted at the cursor, even if its bytes arrive
      in separate {!feed} calls. A mark joins the cluster before the cursor.
    - Backspace erases the cluster before the cursor.
    - The left and right arrows step over one cluster.

    A cluster takes as many cells as a terminal uses to draw it: two for a wide
    character or an emoji. So cursor motion is exact for them.

    The keys, the history and the completion follow those of the
    {{:https://erratique.ch/software/down}down} toplevel line editor. *)

type t
(** The type for line editors: the current line and the history of submitted
    lines. *)

val v :
  ?prompt:string ->
  ?history:string list ->
  ?complete:(string -> string list) ->
  ?mask:string ->
  ?max_line:int ->
  ?history_size:int ->
  unit ->
  t
(** [v ()] is a line editor with an empty line, such that:

    - [prompt] (defaults to [""]) is the prompt. When Tab lists several
      completions, the editor writes [prompt] again, then the current line.
      Leave it empty when the caller writes no prompt;
    - [history] is the list of lines that the up arrow recalls, oldest first
      (defaults to [[]]);
    - [complete word] is the list of completions of [word], the last word of the
      line, when Tab is pressed. The editor extends [word] to the longest prefix
      that the completions share. It lists them when more than one is left.
      Without [complete], Tab does nothing;
    - [mask], if given, is echoed once for each cluster typed, instead of the
      cluster;
    - [max_line] is the maximum number of bytes in a line (defaults to [4095],
      the canonical-mode line length of the Linux tty line discipline). A
      character that would make the line longer is refused whole, and the bell
      ["\007"] is echoed, as readline does;
    - [history_size] is the maximum number of lines that {!history} keeps
      (defaults to [500], bash's [HISTSIZE]). The oldest lines are dropped
      first. The [history] seed counts toward it.

    Whatever is typed, the editor holds a line of at most [max_line] bytes, and
    at most [history_size] submitted lines. Each submitted line has at most
    [max_line] bytes, except lines from the seed.

    A control character in [prompt], [history], [mask] or a completion is echoed
    as a space, so none of them can move the cursor or change colours.

    With [mask], {!pending} and the lines that {!feed} returns hold the bytes
    exactly as typed, except the editing keys. {!feed} keeps a byte above [0x7F]
    that is not part of a character, a C1 control, and a mark with nothing to
    join. It does not turn them into a replacement character, a space, or
    nothing. A masked editor is for a password or a token. It completes nothing
    and recalls nothing. Its {!history} stays empty: it drops the [history] seed
    and keeps no line that it submits. *)

val feed : t -> string -> string * string list
(** [feed t input] reads each byte of [input] in turn, and is [(echo, lines)].
    [echo] is the bytes to write back to the terminal. [lines] are the lines
    that [input] completes, in order, each without its terminator. The bytes act
    as follows:

    - a printable character is inserted at the cursor and echoed. If the line
      has no room for it ([max_line]), the bell rings instead. A printable
      character is ASCII \[[0x20];[0x7E]\], or a multi-byte UTF-8 character
      whose bytes may arrive in several [feed] calls. A zero-width mark with
      nothing before it in the line to join is dropped, as a terminal drops it
      in the first column. Bytes that decode to no character become replacement
      characters U+FFFD, one for each maximal part of a character, as
      {!Console_text.sanitize} shows them. A terminal would act on a C1 control
      (U+0080 to U+009F), so it becomes a space. So what is echoed and kept is
      only printable text;
    - [\127] (DEL) and [\008] (BS) erase the cluster before the cursor. At the
      end of the line, they echo ["\b \b"];
    - ['\r'] and ['\n'] submit the line. But a ['\n'] right after a ['\r'] (the
      second half of a CRLF) submits nothing;
    - [\t] completes the current word ({!v});
    - [ESC \[ A] and [ESC \[ B] recall the previous and the next line of the
      history;
    - [ESC \[ C] and [ESC \[ D] move the cursor right and left over one cluster.
      A terminal in application cursor mode sends [ESC O A] to [ESC O D]
      instead, with the same effect;
    - any other byte is ignored.

    The cursor always stands between two grapheme clusters of the line. A typed
    character can join the cluster after the cursor, as a ZWJ typed before an
    emoji does. An erased cluster can let the clusters on either side join. In
    both cases, the cursor moves after the joined cluster.

    Feeding [input] at once or in pieces gives the same echo and lines. The echo
    holds no escape code, because the cursor moves with backspaces. *)

val pending : t -> string
(** [pending t] is the current line, the bytes typed since the last submitted
    line. *)

val crlf : string -> string
(** [crlf s] is [s] with a carriage return before each line feed ['\n'], as the
    [ONLCR] output flag of the tty line discipline does. Text written back to a
    peer whose terminal is in raw mode then starts each line in the first
    column. A carriage return already before a line feed is kept, so ["\r\n"]
    becomes ["\r\r\n"], as with [ONLCR]. *)

val history : t -> string list
(** [history t] is the [history] seed, then the submitted lines, oldest first. A
    submitted line equal to the line before it is dropped. *)
