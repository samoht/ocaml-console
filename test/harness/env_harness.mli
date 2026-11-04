(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The environment of a test. *)

val with_env : (string * string) list -> (unit -> 'a) -> 'a
(** [with_env bindings f] is [f ()] with [bindings] in the environment, and puts
    the variables back once [f] ends. [Unix.putenv] cannot unset a variable: one
    that was unset comes back empty, which [Cast], [Colour] and [Recording.size]
    all read as unset. *)
