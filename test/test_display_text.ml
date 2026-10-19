(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_bytes () =
  let check = Alcotest.(check string) in
  check "zero" "0 B" (Fmt.str "%a" Display.pp_bytes 0L);
  check "small" "512 B" (Fmt.str "%a" Display.pp_bytes 512L);
  check "kib" "1.50 KiB" (Fmt.str "%a" Display.pp_bytes 1536L);
  check "mib" "30.00 MiB" (Fmt.str "%a" Display.pp_bytes (mib 30));
  check "gib" "2.00 GiB" (Fmt.str "%a" Display.pp_bytes (gib 2))

let test_span () =
  let check = Alcotest.(check string) in
  check "zero" "0.0s" (Fmt.str "%a" Display.pp_span 0.);
  check "negative" "0.0s" (Fmt.str "%a" Display.pp_span (-1.));
  check "tenths" "6.4s" (Fmt.str "%a" Display.pp_span 6.44);
  check "under a minute" "59.9s" (Fmt.str "%a" Display.pp_span 59.94);
  check "minutes" "1m05s" (Fmt.str "%a" Display.pp_span 65.);
  check "hours" "1h02m" (Fmt.str "%a" Display.pp_span 3725.)

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
      Alcotest.test_case "bytes" `Quick test_bytes;
      Alcotest.test_case "span" `Quick test_span;
      Alcotest.test_case "info event preserves text" `Quick
        test_info_event_preserves_text;
      Alcotest.test_case "control sanitization" `Quick test_sanitizes_controls;
    ] )
