(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** An emulated terminal, and what a terminal recording put on its screen.

    The tests read console's output from the screen of an emulator, matrix's
    [Vte]. It parses bytes as xterm does and sizes grapheme clusters as Ghostty
    does. For a few clusters, Ghostty's size differs from console's, and the
    emulator cannot tell which is right ({!counts_as_console}).

    A recording of a program that redraws lines in place holds every redraw, so
    reading it as text does not say what the screen showed. {!frames} replays a
    recording and reads the screen after each write, with the time of the write.
*)

(** {1:terminal Terminal} *)

type t
(** The type for emulated terminals. *)

val v : ?raw:bool -> rows:int -> cols:int -> unit -> t
(** [v ~rows ~cols ()] is a blank terminal of [rows] by [cols] cells, with its
    cursor at the top left. A line feed also moves the cursor to the first
    column, as a tty with output processing on does. With [raw] (default
    [false]), a line feed keeps the column, as with output processing off. *)

val feed : t -> string -> unit
(** [feed t s] makes [t] read [s]. If [s] ends inside an escape sequence or a
    character, the rest is read from the next [feed]. *)

val vte : t -> Vte.t
(** [vte t] is the emulator behind [t]. *)

val rows : t -> int
(** [rows t] is the height of [t]. *)

val cols : t -> int
(** [cols t] is the width of [t]. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] gives [t] a new size and blanks its screen. *)

val cursor_pos : t -> int * int
(** [cursor_pos t] is the cursor's row and column. After a character in the last
    column, the column is the width of [t]: the next character wraps. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is [true] iff the cursor is shown. *)

val counts_as_console : string -> bool
(** [counts_as_console s] is [true] iff the emulator gives every grapheme
    cluster of [s] as many cells as console counts. It is [false] in two cases:
    - A cluster that is wider than two cells because of its spacing marks, such
      as U+1F469 U+093F or U+FFFD U+302E. Console counts three cells, like
      xterm, VTE and iTerm2. The emulator counts two, like Ghostty.
    - A Hangul vowel or final consonant outside a syllable, or any code point
      from U+D7B0 to U+D7FF. Console counts one cell, like kitty and iTerm2. The
      emulator counts none.

    [test/width/widths.txt] records what each terminal draws. Tests that compare
    console's widths with the screen skip the inputs where this is [false]. *)

val cursor_history : string -> bool * bool
(** [cursor_history s] is [(hidden, shown)] for a fresh terminal that reads [s]
    one byte at a time. [hidden] is [true] iff the cursor was hidden at some
    point, and [shown] iff it is shown at the end. A terminal starts with its
    cursor shown, so [shown] means that a hidden cursor was shown again only
    when [hidden] is [true]. *)

val synchronized_open : string -> bool
(** [synchronized_open s] is [true] iff a terminal that read [s] is left in a
    synchronized update: mode 2026 was last set. Matrix does not model this
    mode, so [s] is scanned for it. *)

val to_string : t -> string
(** [to_string t] is the visible screen, a line per row. *)

val scrollback_lines : t -> string list
(** [scrollback_lines t] is what has scrolled off the top, oldest first. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the number of {!scrollback_lines}. *)

val grid : t -> Matrix_grid.t
(** [grid t] is the visible screen's cells. *)

val fg : Matrix_grid.t -> int -> Ansi.Color.t
(** [fg g i] is the foreground colour of cell [i] of [g]. *)

val bg : Matrix_grid.t -> int -> Ansi.Color.t
(** [bg g i] is the background colour of cell [i] of [g]. *)

val screen : ?scrollback:bool -> t -> string list
(** [screen t] is the visible screen: one string per row, without trailing
    blanks or the blank rows at the bottom. With [scrollback] (default [false]),
    the lines that scrolled off the top come first. *)

(** {1:recordings Recordings} *)

val records : string -> (float * string) list
(** [records recording] is what the recorded program wrote and when, oldest
    first. There is one element per write, with the time the capture stamped on
    it, in seconds since the epoch.

    [recording] is the raw byte stream that BSD [script(1) -r] writes. It is a
    sequence of little-endian records:

    {v <uint64 len><int64 tv_sec><int32 tv_usec><int32 kind><len bytes> v}

    [kind] is ['s'] for the session header, ['o'] for a write of the program,
    and ['e'] for the end. Only the ['o'] records are returned. A trailing
    partial record is dropped, because a capture of a program that was still
    running ends with one. It does not read asciicast, or the output of
    util-linux's [script], which keeps its timings in a second file. *)

val frames : rows:int -> cols:int -> string -> (float * string list) list
(** [frames ~rows ~cols recording] is the screen after each write in
    [recording], with its timestamp. It replays the writes of {!records} into a
    [rows] by [cols] terminal, and takes {!screen} right after each one.

    The timestamp is in seconds since the first record of the recording, not
    since the epoch as in {!records}. So two captures of the same command are
    comparable, and a diff of one is stable. Empty writes make no frame, because
    they change nothing on the screen. *)
