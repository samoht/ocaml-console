(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Direct-style fibers on Lwt.

    [Console_driver] waits by calling a function that returns when the wait is
    over. A fiber runs such code as an Lwt promise. Each {!await} in the fiber
    suspends it until the awaited promise resolves. The rest of the fiber then
    runs in that promise's callback. The fiber's promise is the chain of its
    waits. So [Lwt.cancel] on it cancels the promise that the fiber waits on,
    and {!await} raises [Lwt.Canceled] in the fiber. *)

val spawn : (unit -> 'a) -> 'a Lwt.t
(** [spawn f] is the promise of [f ()], run as a fiber. It resolves with the
    value that [f] returns, or is rejected with the exception that [f] raises.
    When [spawn] returns, the Lwt storage holds what it held before, even if [f]
    bound values before its first wait. *)

val await : 'a Lwt.t -> 'a
(** [await p] is the value of [p] once it resolves. If [p] is rejected,
    [await p] raises the exception. If [p] is pending, the calling fiber waits.
    When the wait ends, the [Lwt.with_value] bindings around it are in place
    again.

    @raise Invalid_argument if [p] is pending and the caller is not in a fiber.
*)
