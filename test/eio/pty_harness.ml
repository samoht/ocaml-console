(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let required () =
  match Sys.getenv_opt "CONSOLE_TEST_REQUIRE_PTY" with
  | None | Some "" -> false
  | Some _ -> true

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      if required () then Alcotest.fail "no pseudo-terminal"
      else Alcotest.skip ()
