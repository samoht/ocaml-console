(** Questions asked on the terminal.

    A prompt writes a question on a formatter and reads the answer, one line,
    from an Eio flow, so a fiber waiting for an answer lets the others run. A
    prompt with nobody to type an answer says so with an [Error] instead of
    waiting for ever.

    While a {!Console.Display} runs, ask inside {!Console.Display.with_output},
    which takes the progress rows off the screen for the time of the question.
*)

type t
(** The type for prompts. *)

val v : ?interactive:bool -> ?ppf:Format.formatter -> _ Eio.Flow.source -> t
(** [v stdin] is a prompt reading answers from [stdin] and writing questions on
    [ppf] (defaults to {!Fmt.stdout}).

    [interactive] defaults to whether [stdin] is a terminal: an Eio flow backed
    by a Unix descriptor for which [isatty] holds. A prompt that is not
    interactive answers [Error] to every question and writes nothing, rather
    than blocking on input nobody will type.

    One {!type-t} is created per program and reused: it owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

val line : t -> string -> (string, [ `Msg of string ]) result
(** [line t question] writes [question] verbatim, flushed and with no newline
    added, and is the next line of input without its terminator (LF or CRLF).
    The answer is returned as typed, spaces included. This is [Error] when [t]
    is not interactive or the input ends before a line feed. *)

val confirm : t -> string -> (bool, [ `Msg of string ]) result
(** [confirm t question] asks [question ^ " [y/N] "] and is [true] for an answer
    of ["y"] or ["yes"], compared after trimming surrounding whitespace and
    ignoring case, and [false] for any other line, an empty one included: the
    capital in [N] is the promise that only an explicit yes goes ahead. This is
    [Error] when [t] is not interactive or the input ends. *)

val expect : t -> string -> string -> (unit, [ `Msg of string ]) result
(** [expect t question word] asks [question] and is [Ok ()] iff the answer is
    exactly [word], byte for byte. It is the gate that makes an operator type
    the name of what is about to be destroyed, so a different answer is [Error]
    saying the answer did not match, and the error never spells [word] out: a
    message that repeats the word hands over the very answer the gate exists to
    ask for. This is [Error] too when [t] is not interactive or the input ends.
*)

val secret : t -> string -> (string, [ `Msg of string ]) result
(** [secret t question] is like {!line} but the answer is not shown: one [*] is
    written for each character typed instead, then a line feed when the answer
    ends, and the answer is never retained. Masking switches echo off on the
    terminal behind [t]'s input, so this is [Error] when that input is not a
    terminal, and when [t] is not interactive or the input ends. The terminal is
    given back as it was found on return, on an exception and on SIGINT or
    SIGTERM. *)
