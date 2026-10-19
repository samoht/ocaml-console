(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* Exercise actual process signals: raising an OCaml exception does not test
   the signal disposition a CLI inherits from its caller. *)
let test_sigint no_color =
  let output =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pty = Eio_unix.Pty.open_pty ~sw () in
    let tty = Eio_unix.Pty.tty pty in
    let child =
      Eio_unix.Process.spawn_unix ~sw
        (Eio.Stdenv.process_mgr env)
        ~env:
          [|
            "TERM=xterm-256color"; ("NO_COLOR=" ^ if no_color then "1" else "");
          |]
        ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
        ~executable:
          (Filename.concat
             (Filename.dirname Sys.executable_name)
             "../test/eio/teardown.exe")
        [ "teardown.exe"; "sigint" ]
    in
    let clock = Eio.Stdenv.clock env in
    let written = Buffer.create 256 and buf = Cstruct.create 4096 in
    let source = Eio_unix.Pty.source pty in
    let exited = ref false in
    let rec drain () =
      match
        Eio.Time.with_timeout clock 0.1 (fun () ->
            Ok (Eio.Flow.single_read source buf))
      with
      | Ok n ->
          Buffer.add_string written (Cstruct.to_string ~len:n buf);
          drain ()
      | Error `Timeout -> if not !exited then drain ()
      | exception (End_of_file | Eio.Io _) -> ()
    in
    Eio.Fiber.both
      (fun () ->
        ignore (Eio.Process.await child);
        exited := true)
      drain;
    Buffer.contents written
  in
  check (String.contains output '\027');
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte output;
  if not (Vte.cursor_visible vte) then
    failf "SIGINT left the cursor hidden: %S" output

let suite =
  ("session", [ test_case "SIGINT restores the cursor" [ bool ] test_sigint ])
