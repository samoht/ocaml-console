(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let counting_reporter count =
  let report _src _level ~over k _msgf =
    incr count;
    over ();
    k ()
  in
  { Logs.report }

let with_logs_state f =
  let reporter = Logs.reporter () and level = Logs.level () in
  Fun.protect f ~finally:(fun () ->
      Logs.set_reporter reporter;
      Logs.set_level level)

let test_reporter_scope () =
  with_logs_state @@ fun () ->
  let original = counting_reporter (ref 0) in
  Logs.set_reporter original;
  let buffer = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      Alcotest.(check bool)
        "wrapped while active" true
        (Logs.reporter () != original);
      let task = Display.task display "work" in
      Display.succeed task);
  Alcotest.(check bool) "restored after run" true (Logs.reporter () == original)

let suite =
  ( "display_reporter",
    [ Alcotest.test_case "reporter scope" `Quick test_reporter_scope ] )
