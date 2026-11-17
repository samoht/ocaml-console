(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The file of a recording that [CONSOLE_CAST] asks for. *)

val end_recording : unit -> unit
(** [end_recording ()] ends the current recording, which closes its file and
    restores its formatters. *)

val with_file : (string -> 'a) -> 'a
(** [with_file f] is [f file] on a new temporary [.cast] file. When [f] ends, it
    ends the recording and removes [file]. Windows cannot remove a file that is
    open. *)
