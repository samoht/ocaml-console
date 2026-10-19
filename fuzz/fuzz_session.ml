(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* Exercise actual process signals: raising an OCaml exception does not test
   the signal disposition a CLI inherits from its caller. *)
let capture mode no_color =
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
      [ "teardown.exe"; mode ]
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

let test_sigint no_color =
  let output = capture "sigint" no_color in
  check (String.contains output '\027');
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte output;
  if not (Vte.cursor_visible vte) then
    failf "SIGINT left the cursor hidden: %S" output

let test_teardown n no_color =
  let modes = [| "ok"; "raise"; "cancel"; "exit" |] in
  let output = capture modes.(n mod Array.length modes) no_color in
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte output;
  check (Vte.cursor_visible vte);
  check_eq ~pp:Fmt.int 0 (snd (Vte.cursor_pos vte));
  let y, _ = Vte.cursor_pos vte in
  Vte.feed_string vte "AFTER";
  let g = Vte.grid vte in
  let i = Grid.idx g ~x:0 ~y in
  let plain = Vte.create ~rows:1 ~cols:80 () in
  Vte.feed_string plain "AFTER";
  let reference = Vte.grid plain in
  let j = Grid.idx reference ~x:0 ~y:0 in
  check (Grid.get_fg_r g i = Grid.get_fg_r reference j);
  check (Grid.get_fg_g g i = Grid.get_fg_g reference j);
  check (Grid.get_fg_b g i = Grid.get_fg_b reference j);
  check (Grid.get_bg_r g i = Grid.get_bg_r reference j)

let domain_source = Logs.Src.create "console.adversarial"

let domain_records reporter prefix count =
  for i = 0 to count - 1 do
    reporter.Logs.report domain_source Logs.Info
      ~over:(fun () -> ())
      (fun () -> ())
      (fun m -> m "%s%04d" prefix i)
  done

let check_records output prefix count =
  let re = Re.compile (Re.str prefix) in
  let records =
    String.split_on_char '\n' output
    |> List.filter_map (fun line ->
        match Re.exec_opt re line with
        | None -> None
        | Some group ->
            let i, _ = Re.Group.offset group 0 in
            Some (String.sub line i (String.length line - i)))
  in
  let expected = List.init count (fun i -> Fmt.str "%s%04d" prefix i) in
  check_eq ~pp:Fmt.(Dump.list string) expected records

let test_concurrent_sessions n =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pool =
    Eio.Executor_pool.create ~sw ~domain_count:2 (Eio.Stdenv.domain_mgr env)
  in
  let count = 1 + (n mod 20) in
  let output = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer output in
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock ~ppf (fun display ->
      let reporter = Logs.reporter () in
      let fibers =
        List.init 2 (fun worker () ->
            Console_eio.run ~clock (fun nested ->
                check (nested == display);
                let task =
                  Console.Display.task nested ("fiber" ^ string_of_int worker)
                in
                for i = 0 to count - 1 do
                  Console.Display.log task (Fmt.str "event%04d" i);
                  Eio.Fiber.yield ()
                done;
                Console.Display.succeed task))
      in
      let domains =
        List.init 2 (fun worker () ->
            Eio.Executor_pool.submit_exn pool ~weight:1. (fun () ->
                domain_records reporter
                  ("domain" ^ string_of_int worker ^ "_event")
                  count))
      in
      Eio.Fiber.all (fibers @ domains));
  Format.pp_print_flush ppf ();
  let output = Buffer.contents output in
  check (String.is_valid_utf_8 output && not (String.contains output '\027'));
  for worker = 0 to 1 do
    check_records output ("fiber" ^ string_of_int worker ^ ": event") count;
    check_records output ("domain" ^ string_of_int worker ^ "_event") count
  done

let suite =
  ( "session",
    [
      test_case "nested fibers and domains retain every log in order" [ uint8 ]
        test_concurrent_sessions;
      test_case "return exception cancellation and exit restore the terminal"
        [ uint8; bool ] test_teardown;
      test_case "SIGINT restores the cursor" [ bool ] test_sigint;
    ] )
