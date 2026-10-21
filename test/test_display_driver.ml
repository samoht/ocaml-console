(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let check_lines msg expected actual =
  Alcotest.(check (list string)) msg expected actual

let theme = Theme.dos
let split = String.split_on_char '\n'

(* [untimed l] is [l] without the timer at its end. *)
let untimed = Re.replace_string (Re.Perl.compile_pat " +[0-9.]+s$") ~by:""

(* A Logs message inside a task is its output, as a line given to [log]
   is: it shows in the task's window, goes when the task succeeds, even
   once pushed out of the window, and is printed under the task's line,
   after the mark of its level, when it fails. *)
let test_logs_in_task () =
  let run ~fails =
    let t = terminal ~theme ~width:30 ~height:10 () in
    Display.with_task ~log_lines:1 t.display "job" (fun job ->
        Driver.report t.display `Warning "first";
        Display.log job "out";
        Display.redraw t.display;
        Alcotest.(check bool)
          "pushed out, not above" false
          (List.exists (contains "first") (screen t));
        Driver.report t.display `Info "last";
        if fails then Display.fail job);
    Display.close t.display;
    List.map untimed (lines t)
  in
  check_lines "a success folds them away" [ "[+] job" ] (run ~fails:false);
  check_lines "a failure prints them all under its line"
    [ "[x] job"; "  ! first"; "  # out"; "  last" ]
    (run ~fails:true)

(* A Logs message with no current task goes above at once, a
   debug message after its own mark, an application or an information
   message after none. A task started with [task] is not current. *)
let test_logs_outside () =
  let report d =
    ignore (Display.task d "running");
    List.iter
      (fun (level, text) -> Driver.report d level text)
      [
        (`App, "app");
        (`Info, "info");
        (`Debug, "debug");
        (`Warning, "warn");
        (`Error, "error");
      ]
  in
  let expected = [ "app"; "info"; ". debug"; "! warn"; "x error" ] in
  let t = terminal ~theme ~width:30 ~height:10 () in
  report t.display;
  Display.redraw t.display;
  check_lines "above the rows, at once" expected
    (List.filteri (fun i _ -> i < 5) (screen t));
  Display.close t.display;
  let (), out = plain ~theme (fun d _ -> report d) in
  check_lines "on a pipe"
    (("[-] running" :: expected) @ [ "[~] running  0.0s  cancelled"; "" ])
    (split out);
  let (), out = plain (fun d _ -> Driver.report d `Debug "debug") in
  check_lines "the default theme's debug mark" [ "\u{00b7} debug"; "" ]
    (split out)

(* A driver's scope says which task is current, here a fiber's own. *)
let test_scope () =
  let current = ref None in
  let scope =
    Driver.scope
      ~current:(fun () -> !current)
      ~within:(fun task f ->
        let saved = !current in
        current := Some task;
        Fun.protect f ~finally:(fun () -> current := saved))
      ~cancelled:(fun _ -> false)
  in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Driver.v ~scope ~now:(fun () -> 0.) ppf in
  let a = Display.task d "a" in
  current := Some a;
  Driver.report d `Info "in a";
  current := None;
  let is task = function Some c -> c == task | None -> false in
  Display.with_task d "b" (fun b ->
      Alcotest.(check bool) "current within" true (is b !current);
      Alcotest.(check bool)
        "the display's current task" true
        (is b (Driver.current d));
      Display.succeed b;
      Alcotest.(check bool)
        "none once it ended" true
        (Option.is_none (Driver.current d)));
  Alcotest.(check bool) "none outside" true (Option.is_none (Driver.current d));
  Display.close d;
  Format.pp_print_flush ppf ();
  check_lines "the scope's current task"
    [
      "[-] a"; "  a: in a"; "[-] b"; "[+] b  0.0s"; "[~] a  0.0s  cancelled"; "";
    ]
    (split (Buffer.contents buffer))

let suite =
  ( "display_driver",
    [
      Alcotest.test_case "Logs lines inside a task are its output" `Quick
        test_logs_in_task;
      Alcotest.test_case "Logs lines outside a task" `Quick test_logs_outside;
      Alcotest.test_case "a driver's scope" `Quick test_scope;
    ] )
