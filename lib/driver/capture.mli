(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a program writes on a formatter while a display runs.

    Written bytes become output, a whole line at a time. They belong to the task
    current where they are written: each task collects its own lines, and its
    unended line is ended when the task ends. Bytes written outside any task go
    above the region, or with JSON events are output that belongs to no task. *)

type run = { display : Console.Display.t; root : Console.Task.t; json : bool }
(** What takes the bytes. *)

val take : direct:(unit -> bool) -> run -> Format.formatter -> unit
(** [take ~direct run ppf] takes what is written on [ppf], until {!restore}.
    Bytes written while [direct ()] holds go straight out. *)

val taking : unit -> bool
(** [taking ()] is [true] iff a formatter is taken. *)

val flush_all : unit -> unit
(** [flush_all ()] flushes the taken formatters. *)

val end_line : Console.Task.t -> unit
(** [end_line task] ends the unended line of [task], which has ended. *)

val settle : run -> unit
(** [settle run] logs what the formatters still hold, with JSON events. *)

val restore : ?flush:bool -> unit -> unit
(** [restore ()] gives the formatters their functions back and writes out what
    they still hold. With [~flush:false], as from another thread, they only get
    their functions back. *)

val safely : (unit -> unit) -> unit
(** [safely f] runs [f] and drops what it raises, unless that is asynchronous.
*)
