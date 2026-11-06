(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Direct-style fibers on Lwt.

    [Console_driver] waits by calling a function that returns once the wait is
    over. A fiber runs such code as an Lwt promise. Each {!await} in it suspends
    the fiber until its promise resolves, and the rest of the fiber runs in that
    promise's callback. The fiber's promise is the chain of its waits, so
    [Lwt.cancel] on it cancels the promise the fiber waits on, and {!await}
    raises [Lwt.Canceled] in the fiber. *)

val spawn : ?stop:bool Atomic.t -> (unit -> 'a) -> 'a Lwt.t
(** [spawn f] is the promise of [f ()], run as a fiber. It resolves with what
    [f] returns, and is rejected with what [f] raises. Once [stop] (defaults to
    a flag that is never set) is set, a wait of the fiber that ends raises
    {!Stopped} instead of returning. When [spawn] returns, the storage of Lwt
    holds what it held before, whatever [f] bound before its first wait. *)

val await : 'a Lwt.t -> 'a
(** [await p] is the value of [p], once it resolves, or raises what rejects it.
    If [p] is pending, the calling fiber waits, and the [Lwt.with_value]
    bindings around the wait hold again once it ends.

    @raise Invalid_argument if [p] is pending and the caller is not in a fiber.
*)

exception Stopped
(** The exception of a wait that ends after the [stop] of its fiber is set. *)

val enter : unit -> unit
(** [enter ()] makes the calling domain and thread the ones that run Lwt. *)

val at_home : unit -> bool
(** [at_home ()] is [true] iff the caller runs in the domain and thread of the
    last {!enter}. *)
