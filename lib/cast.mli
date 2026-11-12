(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Recordings of a terminal in
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}.

    This module is internal to the [console] package. [Console] does not expose
    it. [console.tty] and [console-eio] use it through [Console__]. *)

val file : unit -> string option
(** [file ()] is the file that [CONSOLE_CAST] names, when it is set and not
    empty. *)

val deterministic : unit -> bool
(** [deterministic ()] is [true] iff [CONSOLE_DETERMINISTIC_CLOCK] is set and
    not empty, or if {!file} names a recording. *)

type header = { width : int; height : int; term : string option }
(** The type for the first line of a recording: the terminal's size, and its
    [TERM] if known. *)

val header : header Jsont.t
(** [header] maps the header line of a recording: [version] 2, the terminal's
    size, and its [TERM] in [env]. [TERM] is written as valid UTF-8, in the same
    way as {!event} writes its data. *)

type event = { time : float; data : string }
(** The type for the bytes written to a terminal at [time], in seconds. *)

val event : event Jsont.t
(** [event] maps an output event to the array [[time, "o", data]], with
    {!field-time} rounded to the microsecond. Each ill-formed UTF-8 sequence in
    {!field-data} (as [String.get_utf_8_uchar] decodes it) is written as one
    U+FFFD, so a recording is valid UTF-8. *)

val to_line : 'a Jsont.t -> 'a -> string
(** [to_line t v] is the JSON of [v] on one line, ending with a line feed. Each
    C1 control is written as [\uXXXX], as asciicast v2 requires for
    non-printable characters. *)
