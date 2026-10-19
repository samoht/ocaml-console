(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let count_occurrences needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec loop i count =
    if i + n > m then count
    else if String.sub haystack i n = needle then loop (i + n) (count + 1)
    else loop (i + 1) count
  in
  if n = 0 then 0 else loop 0 0

let count_rows vte needle =
  List.length (List.filter (contains needle) (terminal_rows vte))

let test_activity_does_not_leak_on_ticks () =
  let vte =
    terminal_run ~height:4 ~header:"BUILD" (fun display advance sync _vte ->
        let task = Display.task ~log_lines:2 display "compile" in
        Display.log task "linking";
        for _ = 1 to 30 do
          advance 0.1;
          Display.tick display;
          sync ()
        done;
        Display.succeed task)
  in
  Alcotest.(check int) "header appears once" 1 (count_rows vte "BUILD")

(* The producer's pace is not the eye's: a writer reporting every few
   kilobytes of a large file paints the terminal at most once per 100 ms
   between the driver's ticks, the cadence buildkit's and cargo's progress
   redraw at, and the tick that follows draws the frame the last report
   left. On a pipe nothing between the reports is drawn at all. *)
let test_reports_between_ticks_paint_at_the_eyes_pace () =
  let buffer = Buffer.create 65536 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let reports = 1000 in
  let total = mib (reports + 1) in
  let frames () =
    Format.pp_print_flush ppf ();
    count_occurrences "disk image" (Buffer.contents buffer)
  in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      let task = Display.task display "disk image" in
      let at_start = frames () in
      (* A report a millisecond for a second: the writer's cadence. *)
      for i = 1 to reports do
        advance 0.001;
        Display.set_bytes task ~cur:(mib i) ~total
      done;
      let painted = frames () - at_start in
      (* Ten frames in the second, and one the first report may paint. *)
      let most = 11 in
      Alcotest.(check bool)
        (Fmt.str "%d reports in a second paint at most %d frames, not %d"
           reports most painted)
        true (painted <= most);
      Alcotest.(check bool)
        "and more than one: the screen moved during the second" true
        (painted > 1);
      (* The driver paints, so the next report lands inside the interval. *)
      Display.tick display;
      advance 0.05;
      Display.set_bytes task ~cur:total ~total;
      Format.pp_print_flush ppf ();
      (* The current figure ahead of the total, which every frame names. *)
      let last = Fmt.str "%a / " Display.pp_bytes total in
      Alcotest.(check bool)
        "a report inside the interval is not yet on screen" false
        (contains last (Buffer.contents buffer));
      Display.tick display;
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the tick draws it" true
        (contains last (Buffer.contents buffer));
      Display.succeed task)

let test_activity_collapse () =
  let newest_visible = ref false and marker_visible = ref false in
  let _vte =
    terminal_run ~height:6 ~header:"[H]" (fun display _ sync vte ->
        let tasks =
          List.init 20 (fun i -> Display.task display (Fmt.str "[R%d]" (i + 1)))
        in
        Display.tick display;
        sync ();
        let screen = Vte.to_string vte in
        newest_visible := contains "[R20]" screen;
        marker_visible := contains "more lines)" screen;
        List.iter Display.cancel (List.rev tasks))
  in
  Alcotest.(check bool) "newest activity was visible" true !newest_visible;
  Alcotest.(check bool) "overflow marker was visible" true !marker_visible

let test_history_survives_activity () =
  let vte =
    terminal_run ~height:4 ~width:60 ~header:"[H]" (fun display _ sync _vte ->
        for i = 1 to 12 do
          let task = Display.task display (Fmt.str "[D%d]" i) in
          Display.succeed task;
          sync ()
        done)
  in
  for i = 1 to 12 do
    Alcotest.(check int)
      (Fmt.str "event D%d exactly once" i)
      1
      (count_rows vte (Fmt.str "[D%d]" i))
  done

(* A log line is on the screen once: in the row's live tail while the row
   runs, and in permanent history once it ages out of the tail or the row
   finishes. *)
let test_log_reaches_the_terminal_once () =
  ignore
    (terminal_run ~height:8 ~width:60 ~header:"[H]"
       (fun display _advance sync vte ->
         let task = Display.task ~log_lines:4 display "source" in
         Display.log task "checkout cache hit";
         sync ();
         Alcotest.(check int)
           "one copy while the row runs" 1
           (count_rows vte "checkout cache hit");
         Display.succeed task;
         sync ();
         Alcotest.(check int)
           "one copy once the row is done" 1
           (count_rows vte "checkout cache hit")))

(* A window line is a live stream watched through the row's tail: never more
   lines than the tail holds, and nothing left behind, on the screen or in the
   history, once the row ends. *)
let kernel_line i =
  Fmt.str "[    0.%06d] pci 0000:00:%02d.0: [1af4:1041] type 00 class 0x020000"
    (i * 1733) i

let test_a_window_line_never_reaches_history () =
  let width = 60 and window = 8 in
  let logged = ref [] in
  let vte =
    terminal_run ~height:14 ~width (fun display _advance sync vte ->
        let task = Display.task ~log_lines:window display "Target: booting" in
        for i = 1 to 30 do
          Display.log ~keep:false task (kernel_line i);
          sync ();
          let shown =
            List.length
              (List.filter (contains "pci 0000")
                 (String.split_on_char '\n' (Vte.to_string vte)))
          in
          if shown > window then
            Alcotest.failf "after %d lines the window is %d lines high" i shown
        done;
        Display.succeed task;
        logged :=
          List.filter
            (fun event ->
              match Display.Event.kind event with
              | `Log _ -> true
              | `Started | `Message _ | `Finished _ | `Result _ -> false)
            (Display.history display))
  in
  Alcotest.(check int)
    "no window line is left on the screen or in scrollback" 0
    (List.length (List.filter (contains "pci 0000") (terminal_rows vte)));
  Alcotest.(check int) "and none is in the history" 0 (List.length !logged)

let test_an_appending_display_writes_no_window () =
  let (), output =
    capture (fun display _ ->
        let task = Display.task display "Target: booting" in
        Display.log ~keep:false task (kernel_line 1);
        Display.succeed task)
  in
  Alcotest.(check bool)
    "a pipe gets the row and not the window" false
    (contains "pci 0000" output);
  Alcotest.(check bool)
    "but it gets the row" true
    (contains "Target: booting" output)

let test_activity_lines_are_bounded () =
  let vte =
    terminal_run ~height:5 ~width:18 ~header:"HEADER-UNIQUE-LONG"
      (fun display _ sync _vte ->
        let task =
          Display.task display
            "a very long task label with 你好 and control\tbytes"
        in
        Display.tick display;
        sync ();
        Display.succeed task)
  in
  Alcotest.(check bool)
    "no duplicate header from wrapping" true
    (count_rows vte "HEADER-" <= 1)

let test_tiny_activity_is_bounded () =
  let did_scroll = ref false in
  let _vte =
    terminal_run ~height:1 ~width:20 ~header:"TINY"
      (fun display advance sync vte ->
        let task = Display.task display "work" in
        for _ = 1 to 20 do
          advance 0.1;
          Display.tick display;
          sync ()
        done;
        did_scroll := Vte.scrollback_lines vte <> [];
        Display.succeed task)
  in
  Alcotest.(check bool) "activity never scrolled" false !did_scroll

let suite =
  ( "display_projection",
    [
      Alcotest.test_case "activity tick does not leak" `Quick
        test_activity_does_not_leak_on_ticks;
      Alcotest.test_case "reports between ticks paint at the eye's pace" `Quick
        test_reports_between_ticks_paint_at_the_eyes_pace;
      Alcotest.test_case "activity collapse" `Quick test_activity_collapse;
      Alcotest.test_case "history survives activity" `Quick
        test_history_survives_activity;
      Alcotest.test_case "a log line is drawn once" `Quick
        test_log_reaches_the_terminal_once;
      Alcotest.test_case "a window line never reaches history" `Quick
        test_a_window_line_never_reaches_history;
      Alcotest.test_case "an appending display writes no window" `Quick
        test_an_appending_display_writes_no_window;
      Alcotest.test_case "activity lines bounded" `Quick
        test_activity_lines_are_bounded;
      Alcotest.test_case "tiny activity bounded" `Quick
        test_tiny_activity_is_bounded;
    ] )
