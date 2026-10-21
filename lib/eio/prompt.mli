(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Questions asked on the terminal.

    A prompt writes a question on a formatter and reads the answer, one line,
    from an Eio flow, so a fiber waiting for an answer lets the others run. The
    terminal edits the line as the user types it; {!secret} alone switches the
    terminal's echo off and edits the line with a {!Console_input} editor that
    shows a [*] for each character. A prompt with nobody to type an answer says
    so with an [Error] instead of waiting for ever.

    While a display that [Console_eio.run] opened runs, a question takes its
    progress rows off the screen for the time it is asked. *)

type t
(** The type for prompts. *)

val v : ?interactive:bool -> ?ppf:Format.formatter -> _ Eio.Flow.source -> t
(** [v stdin] is a prompt reading answers from [stdin] and writing questions on
    [ppf] (defaults to {!Fmt.stderr}, so that standard output stays the
    program's), [interactive] or not (defaults to whether [stdin] is a terminal:
    an Eio flow backed by a Unix descriptor for which [isatty] holds). A prompt
    that is not interactive answers [Error] to every question and writes
    nothing, rather than blocking on input nobody will type.

    One {!type-t} is created per program and reused: it owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

val line : t -> string -> (string, [ `Msg of string ]) result
(** [line t question] writes [question], each control character but a line feed
    as a space, flushed and with no newline added, and is the next line of input
    without its terminator (LF or CRLF). The answer is returned as typed, spaces
    included. This is [Error] when [t] is not interactive or the input ends
    before a line feed. *)

val confirm : t -> string -> (bool, [ `Msg of string ]) result
(** [confirm t question] asks [question ^ " [y/N] "] and is [true] for an answer
    of ["y"] or ["yes"], compared after trimming surrounding whitespace and
    ignoring case, and [false] for any other line, an empty one included. This
    is [Error] when [t] is not interactive or the input ends. *)

val secret : t -> string -> (string, [ `Msg of string ]) result
(** [secret t question] is like {!line} but the answer is not shown: one [*] is
    written for each character typed instead, then a line feed when the answer
    ends, and the answer is never retained. The answer is the bytes typed, as
    they were typed, less the keys that edit the line ({!Console_input.feed}).
    Masking switches echo off on the terminal behind [t]'s input, so this is
    [Error] when that input is not a terminal, and when [t] is not interactive
    or the input ends. What is typed before the question is masked, which the
    terminal shows, is discarded, as getpass(3) does. The terminal is given back
    as it was found on return, on an exception and on SIGINT or SIGTERM. *)
