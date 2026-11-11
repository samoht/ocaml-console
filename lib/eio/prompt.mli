(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Questions asked on the terminal.

    A prompt writes a question on a formatter and reads the answer, one line,
    from an Eio flow. So while a fiber waits for an answer, other fibers run.

    The terminal edits the line as the user types it. {!secret} is the
    exception: it switches the terminal's echo off and edits the line with a
    {!Console_input} editor, which shows a [*] for each grapheme cluster typed.
    A prompt with nobody to type an answer returns an [Error] instead of waiting
    for ever.

    While a display opened by [Console_eio.run] is running, a question clears
    its progress rows from the screen until it is answered. Meanwhile, what the
    asking fiber prints goes straight out, and the display captures what other
    fibers print as usual. *)

type t
(** The type for prompts. *)

val v : ?interactive:bool -> ?ppf:Format.formatter -> _ Eio.Flow.source -> t
(** [v stdin] is a prompt that reads answers from [stdin].

    - [ppf] is where it writes questions (defaults to {!Fmt.stderr}, which keeps
      standard output for the program).
    - [interactive] says whether someone can answer (defaults to whether [stdin]
      is a terminal, that is an Eio flow backed by a Unix descriptor for which
      [isatty] is true). A prompt that is not interactive returns [Error] for
      every question and writes nothing, instead of blocking on input that
      nobody will type.

    Create one {!type-t} per program and reuse it. It owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

type error = [ `End_of_input | `Msg of string ]
(** The type for the errors of a question. [`End_of_input] means that the input
    ended before an answer, for example after Ctrl-D on a terminal. [`Msg] is
    any other error, including a prompt that is not {!interactive}. *)

val keys : t -> (Eio.Buf_read.t -> 'a) -> ('a, error) result
(** [keys t f] is [Ok (f input)]. [input] holds the bytes of the keys typed on
    the terminal behind [t]'s input. Each key can be read as soon as it is
    typed, and it is not echoed, as a game needs. A key that sends an escape
    sequence, such as an arrow, appears as those bytes, and [f] decodes them.
    Ctrl-C and Ctrl-Z still send their signals.

    The terminal is restored to its original state when [f] returns or raises,
    when its fiber is cancelled, on SIGINT or SIGTERM, and while SIGTSTP stops
    the program. The rows of a display stay on the screen while [f] runs. The
    result is [Error `End_of_input] when [f] raises [End_of_file], as its reads
    do at the end of the input. It is [Error (`Msg _)] when the input is not a
    terminal, when [t] is not {!interactive}, or on a [Unix.Unix_error]. Any
    other exception from [f] is raised again. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error, [`End_of_input] as ["end of input"] and
    [`Msg m] as [m]. *)

val line : t -> string -> (string, error) result
(** [line t question] writes [question], then is the next line of input without
    its terminator (LF or CRLF). The question is flushed with no newline added.
    Each control character in it, except a line feed, is written as a space. The
    answer is returned as typed, spaces included. The result is
    [Error `End_of_input] when the input ends before a line feed. It is
    [Error (`Msg _)] when [t] is not interactive, or when the line is longer
    than 4096 bytes; the rest of that line is then skipped. *)

val confirm : t -> string -> (bool, error) result
(** [confirm t question] asks [question ^ " [y/N] "]. It is [true] for an answer
    of ["y"] or ["yes"], ignoring case and surrounding whitespace, and [false]
    for any other line, including an empty one. Its errors are those of {!line}.
*)

val secret : t -> string -> (string, error) result
(** [secret t question] is like {!line}, but the answer is not shown. Instead,
    one [*] is written for each grapheme cluster typed, then a line feed when
    the answer ends. The answer is the bytes as typed, minus the keys that edit
    the line ({!Console_input.feed}). It is never retained. As with {!line},
    more than 4096 bytes typed before the end of the line gives
    [Error (`Msg _)], and the rest of the line is skipped.

    Masking switches echo off on the terminal behind [t]'s input. So the result
    is also [Error (`Msg _)] when that input is not a terminal. [question] is
    written first, and echo goes off after it. Like getpass(3), [secret] then
    discards the keys typed before echo went off, because the terminal has
    already shown them. The terminal is restored to its original state on
    return, on an exception, and on SIGINT or SIGTERM. *)
