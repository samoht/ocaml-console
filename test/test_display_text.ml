(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_info_event_preserves_text () =
  let (), output =
    capture (fun display _ ->
        Display.event display "CONFLICT in lib/main.ml\n╭───╮\n│ x │\n╰───╯\n")
  in
  Alcotest.(check string)
    "script-compatible physical lines"
    "CONFLICT in lib/main.ml\n╭───╮\n│ x │\n╰───╯\n" output

let test_sanitizes_controls () =
  let events, output =
    capture ~header:"Build\027[2J" (fun display _ ->
        let task = Display.task display "a\tb" in
        Display.log task "x\007y";
        let history = Display.history display in
        Display.succeed task;
        history)
  in
  Alcotest.(check bool)
    "no injected escape" false
    (String.contains output '\027');
  let log =
    List.find
      (fun event ->
        match Display.Event.kind event with `Log _ -> true | _ -> false)
      events
  in
  Alcotest.(check string) "sanitized history" "x y" (Display.Event.message log)

let suite =
  ( "display_text",
    [
      Alcotest.test_case "info event preserves text" `Quick
        test_info_event_preserves_text;
      Alcotest.test_case "control sanitization" `Quick test_sanitizes_controls;
    ] )
