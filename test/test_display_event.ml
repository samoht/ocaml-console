(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_event_history () =
  let events, _ =
    capture (fun display _ ->
        Display.event ~level:`Warning display "cache is cold";
        let parent = Display.task display "build" in
        let child = Display.task ~parent display "compile" in
        Display.log ~level:`Error child "compiler said no";
        Display.fail ~message:"compile failed" child;
        Display.cancel ~message:"dependency failed" parent;
        Display.history display)
  in
  let kinds = List.map Display.Event.kind events in
  Alcotest.(check int) "six events" 6 (List.length events);
  Alcotest.(check bool)
    "typed kinds" true
    (kinds
    = [
        `Message `Warning;
        `Started;
        `Started;
        `Log `Error;
        `Finished `Failed;
        `Finished `Cancelled;
      ]);
  let log = List.nth events 3 in
  Alcotest.(check (list string))
    "semantic path" [ "build"; "compile" ] (Display.Event.path log);
  Alcotest.(check string)
    "message" "compiler said no"
    (Display.Event.message log);
  Alcotest.(check bool)
    "task id present" true
    (Option.is_some (Display.Event.task log))

let test_logs_are_not_activity () =
  let events, output =
    capture (fun display _ ->
        let task = Display.task ~log_lines:1 display "work" in
        Display.log task "first";
        Display.log task "second";
        let events = Display.history display in
        Display.succeed task;
        events)
  in
  let logs =
    List.filter
      (fun event ->
        match Display.Event.kind event with `Log _ -> true | _ -> false)
      events
  in
  Alcotest.(check int) "both logs retained" 2 (List.length logs);
  Alcotest.(check bool) "first printed" true (contains "first" output);
  Alcotest.(check bool) "second printed" true (contains "second" output)

let test_late_log_is_history () =
  let events, output =
    capture (fun display _ ->
        let task = Display.task ~id:"worker" display "worker" in
        Display.succeed task;
        Display.log ~level:`Warning task "late diagnostic";
        Display.set_count task ~cur:1 ~total:1;
        Display.set_label task "too late";
        Display.history display)
  in
  Alcotest.(check bool)
    "late task log retained" true
    (List.map Display.Event.kind events
    = [ `Started; `Finished `Succeeded; `Log `Warning ]);
  Alcotest.(check bool)
    "late log printed" true
    (contains "late diagnostic" output);
  Alcotest.(check (option string))
    "late log key" (Some "worker")
    (Display.Event.key (List.hd (List.rev events)))

let suite =
  ( "display_event",
    [
      Alcotest.test_case "event history" `Quick test_event_history;
      Alcotest.test_case "logs remain history" `Quick test_logs_are_not_activity;
      Alcotest.test_case "late logs remain history" `Quick
        test_late_log_is_history;
    ] )
