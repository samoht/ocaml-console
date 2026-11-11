(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The frames of a spinner, shared by {!Theme} and {!Spinner}. *)

val braille : string list
(** See {!Spinner.braille}. *)

val ascii : string list
(** See {!Spinner.ascii}. *)

val still : string list
(** [still] is the one frame of a spinner that does not move. *)

val v : string -> string list -> string list
(** [v fn frames] is [frames], each sanitized to one line: each control
    character becomes a space.

    @raise Invalid_argument naming [fn] if [frames] is empty. *)

val at : string list -> time:float -> string
(** [at frames ~time] is the frame shown [time] seconds in, at ten frames a
    second. It is the first frame when [time] is not positive or not finite. *)
