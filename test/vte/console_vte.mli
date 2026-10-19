(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a terminal recording put on the screen.

    A recording of a program that redraws lines in place holds every redraw, so
    reading it as text does not say what the screen showed. These functions
    replay a recording into a terminal emulator and read the screen after each
    write, with the time of the write. *)

val screen : ?scrollback:bool -> Vte.t -> string list
(** [screen vte] is the emulator's visible screen: one string per row, trailing
    blanks removed, and the blank rows at the bottom dropped. [scrollback]
    (default [false]) puts the lines that have already scrolled off the top in
    front of it. *)

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
