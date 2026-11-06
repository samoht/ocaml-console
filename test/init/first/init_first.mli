(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Every word the module initialisers linked after this one allocate, by the
    source file of the initialiser that allocated it. Linking this library first
    in an executable starts {!Gc.Memprof} at a sampling rate of 1, so each word
    allocated from then on is one sample. A sample is charged to the outermost
    frame of its call stack that has a location, which is the module initialiser
    running. *)

val words : ?through:string -> unit -> (string * int) list
(** [words ?through ()] stops sampling and returns the words charged to each
    source file, as the compiler names it. With [through], only the words
    allocated while a function of the source file [through] was on the call
    stack are counted. *)
