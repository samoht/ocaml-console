(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Questions asked on the terminal.

    A prompt writes a question on a formatter and reads the answer, one line,
    from an Eio flow. A fiber waiting for an answer lets the others run.

    The terminal edits the line as the user types it. {!secret} alone switches
    the terminal's echo off, and edits the line with a {!Console_input} editor
    that shows a [*] for each character. A prompt with nobody to type an answer
    says so with an [Error] instead of waiting for ever.

    While a display that [Console_eio.run] opened runs, a question takes its
    progress rows off the screen for the time it is asked. *)

type t
(** The type for prompts. *)

val v : ?interactive:bool -> ?ppf:Format.formatter -> _ Eio.Flow.source -> t
(** [v stdin] is a prompt that reads answers from [stdin].

    - [ppf] is where it writes questions (defaults to {!Fmt.stderr}, so that
      standard output stays the program's).
    - [interactive] says whether someone can answer (defaults to whether [stdin]
      is a terminal: an Eio flow backed by a Unix descriptor for which [isatty]
      holds). A prompt that is not interactive answers [Error] to every question
      and writes nothing, rather than block on input nobody will type.

    A program creates one {!type-t} and reuses it. It owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

val line : t -> string -> (string, [ `Msg of string ]) result
(** [line t question] is the next line of input, without its terminator (LF or
    CRLF), after writing [question]. The question is flushed, with no newline
    added, and each control character in it but a line feed is a space. The
    answer is returned as typed, spaces included. This is [Error] when [t] is
    not interactive or the input ends before a line feed. *)

val confirm : t -> string -> (bool, [ `Msg of string ]) result
(** [confirm t question] asks [question ^ " [y/N] "]. It is [true] for an answer
    of ["y"] or ["yes"], compared after trimming surrounding whitespace and
    ignoring case. It is [false] for any other line, an empty one included. This
    is [Error] when [t] is not interactive or the input ends. *)

val secret : t -> string -> (string, [ `Msg of string ]) result
(** [secret t question] is like {!line} but the answer is not shown. One [*] is
    written for each grapheme cluster typed instead, then a line feed when the
    answer ends. The answer is the bytes typed, as they were typed, less the
    keys that edit the line ({!Console_input.feed}). It is never retained.

    Masking switches echo off on the terminal behind [t]'s input. So this is
    [Error] when that input is not a terminal, as well as when [t] is not
    interactive or the input ends. Input typed before echo goes off, which the
    terminal has already shown, is discarded, as getpass(3) does. The terminal
    is given back as it was found on return, on an exception and on SIGINT or
    SIGTERM. *)
