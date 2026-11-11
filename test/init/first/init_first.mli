(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Every word allocated by the module initialisers linked after this one,
    grouped by the source file of the initialiser. Linking this library first in
    an executable starts {!Gc.Memprof} at a sampling rate of 1, so each word
    allocated from then on is one sample. A sample is charged to the outermost
    frame of its call stack that has a location. That frame is the running
    module initialiser. *)

val words : ?through:string -> unit -> (string * int) list
(** [words ?through ()] stops sampling and returns the words charged to each
    source file, as the compiler names it. With [through], it counts only the
    words allocated while a function of the source file [through] was on the
    call stack. *)
