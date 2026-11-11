(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The environment of a test. *)

val with_env : (string * string) list -> (unit -> 'a) -> 'a
(** [with_env bindings f] is [f ()] with [bindings] in the environment. It
    restores the variables when [f] ends. [Unix.putenv] cannot unset a variable,
    so a variable that was unset is restored as empty. [Cast], [Colour] and
    [Recording.size] all read an empty variable as unset. *)
