(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a terminal recording put on the screen.

    A capture of a command that draws a live region is a byte stream, and a byte
    stream is not what its reader saw: the region moves the cursor up and
    rewrites, so counting the lines in a capture counts redraws rather than
    rows, and the stream has no clock, so a screen that sat still for a minute
    reads the same as one that never stopped. Only a terminal emulator says what
    the screen held, and only the record boundaries say when.

    Both are here so that a command line grading its own live region reads it
    off the same instrument this library grades its own frames with, rather than
    writing a second one. *)

val screen : ?scrollback:bool -> Vte.t -> string list
(** [screen vte] is the emulator's visible screen: one string per row, trailing
    blanks removed, and the blank rows at the bottom dropped. [scrollback]
    (default [false]) puts the lines that have already scrolled off the top in
    front of it, which a session's permanent history needs and a bounded live
    region does not. *)

val records : string -> (float * string) list
(** [records recording] is what the recorded program wrote and when, oldest
    first: one element per write, carrying the seconds since the epoch that the
    capture stamped it with.

    [recording] is the raw byte stream BSD [script(1) -r] writes, a sequence of

    {v <uint64 len><int64 tv_sec><int32 tv_usec><int32 kind><len bytes> v}

    little-endian throughout, with [kind] ['s'] for the session header, ['o']
    for a write the program made and ['e'] for the end. Only the ['o'] records
    are returned; a trailing partial record is dropped, since a capture of a
    program that was still running ends in one. It does not read util-linux's
    [script], which keeps its timings in a second file, and it does not read
    asciicast. *)

val frames : rows:int -> cols:int -> string -> (float * string list) list
(** [frames ~rows ~cols recording] is the screen after each write in
    [recording], with its timestamp: the writes of {!records} replayed into a
    [rows] by [cols] emulator, each paired with {!screen} taken straight after
    it.

    The timestamp is seconds from the recording's first record, not the epoch
    seconds {!records} carries, so two captures of the same command are
    comparable and a diff over one is stable. Empty writes make no frame: they
    change nothing on the screen. *)
