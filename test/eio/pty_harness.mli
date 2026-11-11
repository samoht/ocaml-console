(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A pseudo-terminal for a test, or the test skipped without one. *)

val open_pty : sw:Eio.Switch.t -> Eio_unix.Pty.t
(** [open_pty ~sw] is a pseudo-terminal. Where none can be opened, as in opam's
    sandbox on macOS, the test is skipped. If [CONSOLE_TEST_REQUIRE_PTY] is set,
    the test fails instead. CI sets it, so that the terminal tests cannot stop
    running without anyone noticing. *)
