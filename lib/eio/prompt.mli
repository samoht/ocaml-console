(** Interactive prompts over Eio flows.

    A prompt writes a question to an output flow and reads the answer as one
    line from an input flow. Both are Eio capabilities, so a fiber waiting for
    an answer suspends instead of parking the whole domain in a blocking read,
    and a test drives the very same code from a string source. Every question is
    result-typed: a prompt with nobody at the other end refuses rather than
    waiting for input that will never be typed. *)

type t
(** The type for prompts: where questions go, and the buffered input answers
    come from. *)

val v :
  ?interactive:bool ->
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  unit ->
  t
(** [v ~stdin ~stdout ()] is a prompt reading answers from [stdin] and writing
    questions to [stdout].

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
    is not interactive or the input ends. *)

val confirm : t -> string -> (bool, [ `Msg of string ]) result
(** [confirm t label] asks [label ^ " [y/N] "] and is [true] for an answer of
    ["y"] or ["yes"], compared after trimming surrounding whitespace and
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
(** [secret t question] is like {!line} but the answer is not echoed: one [*] is
    written for each character typed instead, and the answer is never retained.
    Masking switches echo off on the terminal behind [t]'s input, so this is
    [Error] when that input is not a terminal, and when [t] is not interactive
    or the input ends. The terminal is given back as it was found on return, on
    an exception and on SIGINT or SIGTERM. *)
