(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [tty.exe program args] runs [program] on a terminal of 80 columns and 24
   rows, in the environment of [tty.exe], and prints the screen it leaves,
   the lines scrolled off its top first. *)

let rows = 24
let cols = 80

let environment () =
  let ours = [ "TERM=xterm-256color"; "LANG=en_US.UTF-8"; "NO_COLOR=" ] in
  let name b = List.hd (String.split_on_char '=' b) in
  let names = List.map name ours in
  Array.of_list
    (ours
    @ List.filter
        (fun b -> not (List.mem (name b) names))
        (Array.to_list (Unix.environment ())))

let run argv =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Eio_unix.Pty.open_pty ~sw () in
  Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty pty)
    { rows; cols; xpixel = 0; ypixel = 0 };
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:(environment ())
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:(List.hd argv) argv
  in
  let written = Buffer.create 4096 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  (* A read is never raced against a timeout, which could drop what it
     read: the reader runs until the child has exited and nothing more has
     come for half a second. *)
  let rec read () =
    match Eio.Flow.single_read source buf with
    | n ->
        Buffer.add_string written (Cstruct.to_string ~len:n buf);
        read ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  let rec settle seen =
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
    let now = Buffer.length written in
    if now <> seen then settle now
  in
  Eio.Fiber.first read (fun () ->
      ignore (Eio.Process.await child : Eio.Process.exit_status);
      settle (Buffer.length written));
  ignore (Eio.Process.await child : Eio.Process.exit_status);
  let vte = Vte.create ~rows ~cols () in
  Vte.feed_string vte (Buffer.contents written);
  List.iter print_endline (Console_vte.screen ~scrollback:true vte)

let () =
  match Array.to_list Sys.argv with
  | _ :: (_ :: _ as argv) -> run argv
  | _ ->
      prerr_endline "usage: tty.exe PROGRAM [ARG...]";
      exit 2
