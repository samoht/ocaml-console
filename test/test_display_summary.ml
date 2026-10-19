(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_history_output () =
  let (), output =
    capture ~header:"Building" (fun display advance ->
        let resolve = Display.task display "resolve" in
        advance 1.3;
        Display.succeed resolve;
        let fetch = Display.task display "fetch" in
        Display.set_bytes fetch ~cur:(mib 30) ~total:(gib 2);
        Display.log fetch "downloaded manifest";
        advance 2.;
        Display.succeed fetch)
  in
  Alcotest.(check string)
    "permanent output"
    ("=> resolve  1.3s\n" ^ "  # downloaded manifest\n"
   ^ "=> fetch  30.00 MiB / 2.00 GiB  2.0s\n" ^ "[+] Building 3.3s (2/2)\n")
    output

let test_result () =
  let events, output =
    capture (fun display _ ->
        let task = Display.task display "link" in
        Display.fail task;
        Display.set_result display ~detail:"see build.log\nline two"
          (`Fail "build failed");
        Display.history display)
  in
  Alcotest.(check bool) "detail" true (contains "see build.log" output);
  Alcotest.(check bool) "second detail line" true (contains "line two" output);
  Alcotest.(check bool) "verdict" true (contains "✗ build failed" output);
  let result = List.hd (List.rev events) in
  Alcotest.(check bool)
    "semantic result" true
    (Display.Event.kind result = `Result `Fail);
  Alcotest.(check (option string))
    "semantic detail" (Some "see build.log\nline two")
    (Display.Event.detail result)

(* The verdict and its detail on a live display. They are held until the
   session closes and written under the region it releases, which is the only
   place they are written at all: nothing paints them while the region is up.

   The [`History] twin above is [test_result]. *)
let test_result_live () =
  let _events, output =
    capture_live (fun display _ ->
        let task = Display.task display "link" in
        Display.fail task;
        Display.set_result display ~detail:"see build.log\nline two"
          (`Fail "build failed");
        Display.history display)
  in
  Alcotest.(check bool) "detail" true (contains "see build.log" output);
  Alcotest.(check bool) "second detail line" true (contains "line two" output);
  Alcotest.(check bool) "verdict" true (contains "build failed" output)

let test_result_first_wins () =
  let (), output =
    capture (fun display _ ->
        Display.set_result display (`Ok "done");
        Display.set_result display (`Fail "changed my mind"))
  in
  Alcotest.(check bool) "first verdict" true (contains "done" output);
  Alcotest.(check bool)
    "ignored verdict" false
    (contains "changed my mind" output)

let test_empty_header_pending () =
  let initial = ref "" in
  let buffer = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buffer in
  Display.run
    ~ctx:(ctx ~live:true ~ppf ~now:(fun () -> 0.) ())
    ~header:"Building"
    (fun display ->
      Display.tick display;
      Format.pp_print_flush ppf ();
      initial := Console_text.strip (Buffer.contents buffer));
  Alcotest.(check bool)
    "pending marker before any tasks" true
    (contains "[-] Building 0.0s (0/0)" !initial)

let suite =
  ( "display_summary",
    [
      Alcotest.test_case "empty live header remains pending" `Quick
        test_empty_header_pending;
      Alcotest.test_case "history output" `Quick test_history_output;
      Alcotest.test_case "result" `Quick test_result;
      Alcotest.test_case "result on a live display" `Quick test_result_live;
      Alcotest.test_case "first result wins" `Quick test_result_first_wins;
    ] )
