(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What console makes of the terminal on standard output, as one line of
   [key=value] pairs. A test runs it in a Windows pseudo console. *)

let () =
  Console_tty.setup ();
  let colour =
    match Fmt.style_renderer Fmt.stdout with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let size =
    match Console_tty.size Fmt.stdout with
    | Some size ->
        let w, h = size () in
        Fmt.str "%dx%d" w h
    | None -> "none"
  in
  Fmt.pr "terminal=%b colour=%s utf-8=%b size=%s@."
    (Console_tty.is_terminal Unix.stdout)
    colour (Fmt.utf_8 Fmt.stdout) size
