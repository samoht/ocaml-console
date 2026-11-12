(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a program writes on a formatter while a display runs.

    Written bytes become output one whole line at a time. They belong to the
    task that is current where they are written. Each task collects its own
    lines, and its unended line is ended when the task ends. Bytes written
    outside any task go above the region. With JSON events, they are output of
    no task. *)

type run = { display : Console.Display.t; root : Console.Task.t; json : bool }
(** The run that receives the bytes. *)

val take : direct:(unit -> bool) -> run -> Format.formatter -> unit
(** [take ~direct run ppf] captures what is written on [ppf], until {!restore}.
    Bytes written while [direct ()] is [true] go straight out. *)

val taking : unit -> bool
(** [taking ()] is [true] iff a formatter is captured. *)

val flush_all : unit -> unit
(** [flush_all ()] flushes the captured formatters. *)

val end_line : Console.Task.t -> unit
(** [end_line task] ends the unended line of [task]. Call it when [task] ends.
*)

val settle : run -> unit
(** [settle run] logs what the formatters still hold, if [run] uses JSON events.
*)

val restore : ?flush:bool -> unit -> unit
(** [restore ()] gives the formatters their own output functions back and writes
    out what they still hold. With [~flush:false], for a call from another
    thread, it only gives the functions back. *)

val safely : (unit -> unit) -> unit
(** [safely f] runs [f] and drops any exception that is not asynchronous. *)
