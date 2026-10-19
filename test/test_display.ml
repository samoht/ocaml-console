(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* A transient row is drawn while it runs and leaves no line when it ends;
   the history keeps its start and its ending like any row's. *)
let test_transient_row_leaves_no_line () =
  let events, output =
    capture ~header:"Building" (fun display advance ->
        let live = Display.task ~transient:true display "image assembling" in
        advance 1.3;
        Display.set_label live "image assembling: 12 of 31 MB";
        advance 1.;
        Display.succeed live;
        let kept = Display.task display "built" in
        Display.succeed kept;
        Display.history display)
  in
  Alcotest.(check string)
    "the kept row alone reaches the output"
    ("=> built  0.0s\n" ^ "[+] Building 2.3s (2/2)\n")
    output;
  Alcotest.(check bool)
    "the history holds both rows' start and ending" true
    (List.map Display.Event.kind events
    = [ `Started; `Finished `Succeeded; `Started; `Finished `Succeeded ]);
  let (), live_output =
    capture_live ~header:"Building" (fun display advance ->
        let live = Display.task ~transient:true display "image assembling" in
        advance 1.3;
        Display.succeed live)
  in
  Alcotest.(check bool)
    "a terminal commits no line for it either" false
    (contains "=> image assembling" live_output)

let test_with_task_owns_outcome () =
  let events, _ =
    capture (fun display _ ->
        Display.with_task display "normal" ignore;
        Display.with_task display "handled" (fun task -> Display.fail task);
        (try Display.with_task display "raised" (fun _ -> failwith "boom") with
        | Failure message when String.equal message "boom" -> ()
        | exn -> raise exn);
        Display.history display)
  in
  let outcomes =
    List.filter_map
      (fun event ->
        match Display.Event.kind event with
        | `Finished outcome -> Some outcome
        | _ -> None)
      events
  in
  Alcotest.(check bool)
    "normal succeeds, explicit and raised failures win" true
    (outcomes = [ `Succeeded; `Failed; `Failed ])

let test_terminal_operations_are_idempotent () =
  let events, _ =
    capture (fun display _ ->
        let task = Display.task display "once" in
        Display.succeed task;
        Display.succeed ~message:"duplicate" task;
        Display.fail ~message:"too late" task;
        Display.cancel task;
        Display.history display)
  in
  Alcotest.(check bool)
    "first outcome wins" true
    (List.map Display.Event.kind events = [ `Started; `Finished `Succeeded ]);
  Alcotest.(check string)
    "first message wins" "once"
    (Display.Event.message (List.nth events 1))

let test_parent_settles_children () =
  let events, _ =
    capture (fun display _ ->
        let parent = Display.task display "parent" in
        ignore (Display.task ~parent display "child");
        Display.succeed parent;
        Display.history display)
  in
  let finished =
    List.filter_map
      (fun event ->
        match Display.Event.kind event with
        | `Finished outcome -> Some (Display.Event.path event, outcome)
        | _ -> None)
      events
  in
  Alcotest.(check bool)
    "children first" true
    (List.equal
       (fun (path_a, outcome_a) (path_b, outcome_b) ->
         List.equal String.equal path_a path_b && outcome_a = outcome_b)
       finished
       [ ([ "parent"; "child" ], `Succeeded); ([ "parent" ], `Succeeded) ])

let test_failed_parent_cancels_children () =
  let events, _ =
    capture (fun display _ ->
        let parent = Display.task display "parent" in
        ignore (Display.task ~parent display "child");
        Display.fail parent;
        Display.history display)
  in
  let outcomes =
    List.filter_map
      (fun event ->
        match Display.Event.kind event with
        | `Finished outcome -> Some outcome
        | _ -> None)
      events
  in
  Alcotest.(check bool)
    "child cancelled, parent failed" true
    (outcomes = [ `Cancelled; `Failed ])

(* What a terminal holds once a row ends while the row under it is still
   running, read off the emulator rather than off the byte stream: a live region
   rewrites itself, so counting the writes counts redraws and only the screen
   says what a reader was left with.

   The screen keeps one line, the one naming the work that ended. The row under
   it leaves with it and commits nothing, because it never reported an ending of
   its own and the only sentence the display could write for it would be about
   its own shape -- which row hung under which. A reader watching a command has
   no rows and no children in front of them, only work. *)
let test_a_swept_row_reaches_no_screen () =
  let (), output =
    capture_live (fun display _ ->
        let parent = Display.task display "building partition disk images" in
        ignore (Display.task ~parent display "no output yet");
        Display.succeed parent)
  in
  let vte = Vte.create ~rows:24 ~cols:80 () in
  Vte.feed_string vte output;
  let screen = Console_vte.screen ~scrollback:true vte in
  let line = String.concat "\n" screen in
  Alcotest.(check bool)
    "it names the work that ended" true
    (contains "building partition disk images" line);
  Alcotest.(check bool)
    "nothing about the display's own shape" false (contains "parent" line);
  Alcotest.(check bool)
    "the swept row wrote nothing" false
    (contains "no output yet" line);
  Alcotest.(check int) "one line on the screen" 1 (List.length screen)

let test_task_ownership () =
  let foreign = ref None in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History (fun first ->
      let parent = Display.task first "first" in
      foreign := Some parent;
      Display.succeed parent);
  Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History (fun second ->
      match !foreign with
      | None -> Alcotest.fail "foreign task missing"
      | Some parent ->
          check_invalid
            "Console.Display.task: parent belongs to another display" (fun () ->
              ignore (Display.task ~parent second "second")))

let test_closed_handles () =
  let saved = ref None in
  capture (fun display _ ->
      let task = Display.task display "saved" in
      saved := Some task;
      Display.succeed task)
  |> ignore;
  match !saved with
  | None -> Alcotest.fail "task was not saved"
  | Some task ->
      check_invalid "Console.Display.set_label: closed display" (fun () ->
          Display.set_label task "late")

let test_unfinished_is_succeeded () =
  let saved = ref None in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History (fun display ->
      saved := Some display;
      ignore (Display.task display "forgotten"));
  match !saved with
  | None -> Alcotest.fail "display was not saved"
  | Some display ->
      let events = Display.history display in
      Alcotest.(check int) "start and success recorded" 2 (List.length events);
      Alcotest.(check bool)
        "success outcome" true
        (Display.Event.kind (List.hd (List.rev events)) = `Finished `Succeeded)

let test_exception_cancels_and_propagates () =
  let saved = ref None in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  Alcotest.check_raises "original exception" (Failure "boom") (fun () ->
      Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History (fun display ->
          saved := Some display;
          ignore (Display.task display "doomed");
          failwith "boom"));
  match !saved with
  | None -> Alcotest.fail "display was not saved"
  | Some display ->
      Alcotest.(check bool)
        "cancelled" true
        (Display.history display
        |> List.exists (fun event ->
            Display.Event.kind event = `Finished `Cancelled))

let test_validation () =
  check_invalid "Console.Display.Line.bar: negative width" (fun () ->
      ignore (Display.Line.bar (-1)));
  check_invalid "Console.Display.Line.v: more than one label" (fun () ->
      ignore Display.Line.(v [ label; label ]));
  check_invalid "Console.Display.Line.v: more than one spacer" (fun () ->
      ignore Display.Line.(v [ spacer; spacer ]));
  check_invalid "Console.Display.run: width must be positive" (fun () ->
      Display.run
        ~ctx:(ctx ~ppf:Format.std_formatter ~now:(fun () -> 0.) ())
        ~width:0 ignore);
  check_invalid "Console.Display.run: height must be positive" (fun () ->
      Display.run
        ~ctx:(ctx ~ppf:Format.std_formatter ~now:(fun () -> 0.) ())
        ~height:0 ignore);
  capture (fun display _ ->
      check_invalid "Console.Display.task: negative log_lines" (fun () ->
          ignore (Display.task ~log_lines:(-1) display "bad"));
      check_invalid "Console.Display.task: negative total" (fun () ->
          ignore (Display.task ~total:(-1) display "bad"));
      let task = Display.task display "bounded" in
      check_invalid "Console.Display.set_count: expected non-negative progress"
        (fun () -> Display.set_count task ~cur:(-1) ~total:1);
      check_invalid "Console.Display.advance: negative increment" (fun () ->
          Display.advance ~by:(-1) task);
      check_invalid "Console.Display.advance: negative total" (fun () ->
          Display.advance ~total:(-1) task);
      check_invalid "Console.Display.set_bytes: expected non-negative progress"
        (fun () -> Display.set_bytes task ~cur:(-1L) ~total:1L);
      Display.set_count task ~cur:1 ~total:2;
      check_invalid "Console.Display.set_bytes: task already tracks count"
        (fun () -> Display.set_bytes task ~cur:1L ~total:2L);
      Display.set_result display (`Ok "settled"))
  |> ignore

let test_nested_run_reuses_owner () =
  let buffer = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let c = ctx ~is_tty:true ~ppf ~now () in
  Display.run ~ctx:c ~mode:`Activity (fun outer ->
      Display.run ~ctx:c ~mode:`Activity (fun inner ->
          Alcotest.(check bool) "same activity display" true (outer == inner));
      Display.run ~ctx:c ~mode:`History (fun inner ->
          Alcotest.(check bool) "mode cannot fork display" true (outer == inner));
      let task = Display.task outer "outer" in
      Display.succeed task)

(* The permanent rows a terminal holds: those led, past their indent, by one of
   the default theme's committed markers. *)
let committed_rows vte =
  terminal_rows vte
  |> List.filter (fun row ->
      List.exists
        (fun marker -> String.starts_with ~prefix:marker (String.trim row))
        [ "=>"; "!!"; "--" ])

(* A build draws one group row per service and one row per step beneath it,
   the way BuildKit's TTY progress does. While the group runs, a step that
   ended stays beneath it with its outcome and its frozen timer; when the group
   settles, its line and then its steps' lines leave as one block, the steps
   indented under it. Every row, group or step, lays its trailing cells on the
   block's grid: a seven-column count cell ("256/256", the widest total the
   block carries), two spaces and a six-column timer cell, so a step's cells sit
   in the columns of its group's. The expected cells are that grid, at the two
   widths a terminal is commonly drawn at. *)
let test_group_lays_its_rows_beneath_it width () =
  let live = ref [] in
  let vte =
    terminal_run ~height:12 ~width (fun display advance sync vte ->
        let group = Display.task ~group:true display "service api" in
        let from = Display.task ~parent:group display "[1/3] FROM alpine" in
        Display.set_count from ~cur:2 ~total:2;
        advance 0.4;
        Display.succeed from;
        let run = Display.task ~parent:group display "[2/3] RUN apk add curl" in
        Display.set_count run ~cur:12 ~total:256;
        advance 11.0;
        sync ();
        live :=
          List.filteri
            (fun i _ -> i < 3)
            (String.split_on_char '\n' (Vte.to_string vte));
        Display.set_count run ~cur:256 ~total:256;
        Display.succeed run;
        let copy = Display.task ~parent:group display "[3/3] COPY . ." in
        advance 0.2;
        Display.succeed copy;
        Display.succeed group)
  in
  let cells = "count cell, two spaces, timer cell" in
  let live_expected =
    [
      ("service api", "          11.4s");
      ("[1/3] FROM alpine", "    2/2    0.4s");
      ("[2/3] RUN apk add curl", " 12/256   11.0s");
    ]
  in
  Alcotest.(check int)
    "the group and its two steps are drawn" 3 (List.length !live);
  List.iter2
    (fun (label, trailing_cells) row ->
      Alcotest.(check bool) (label ^ " is drawn") true (contains label row);
      Alcotest.(check int)
        (label ^ " reaches the right edge")
        width (Width.string_width row);
      Alcotest.(check string)
        (label ^ " while the group runs: " ^ cells)
        trailing_cells (trailing 15 row))
    live_expected !live;
  Alcotest.(check bool)
    "a step that ended stays beneath its running group" true
    (String.starts_with ~prefix:"  " (List.nth !live 1));
  let committed = committed_rows vte in
  let expected =
    [
      ("=> service api", "          11.6s");
      ("  => [1/3] FROM alpine", "    2/2    0.4s");
      ("  => [2/3] RUN apk add curl", "256/256   11.0s");
      ("  => [3/3] COPY . .", "           0.2s");
    ]
  in
  Alcotest.(check int)
    "one block: the group and its three steps" (List.length expected)
    (List.length committed);
  List.iter2
    (fun (lead, trailing_cells) row ->
      Alcotest.(check bool)
        (lead ^ " in its place in the block")
        true
        (String.starts_with ~prefix:lead row);
      Alcotest.(check int)
        (lead ^ " reaches the right edge")
        width (Width.string_width row);
      Alcotest.(check string)
        (lead ^ ": " ^ cells)
        trailing_cells (trailing 15 row))
    expected committed

(* A group asked to end while a row under it is still open does not settle:
   it stays running, writes nothing, and settles when that row does, with the
   ending it was asked for rather than one read off its rows (both steps
   succeed, the group fails) and a timer that ran to that moment, 3.5 seconds
   and not the 1.5 at which it was asked. *)
let test_group_settles_with_its_last_row () =
  let history = ref [] in
  let vte =
    terminal_run ~height:8 ~width:80 (fun display advance sync _vte ->
        let group = Display.task ~group:true display "service web" in
        let from = Display.task ~parent:group display "[1/2] FROM nginx" in
        advance 0.5;
        Display.succeed from;
        let copy = Display.task ~parent:group display "[2/2] COPY site" in
        advance 1.0;
        Display.fail ~message:"service web failed" group;
        sync ();
        Alcotest.(check (list string))
          "the group is still running beside its open row"
          [ "service web"; "[2/2] COPY site" ]
          (List.map Display.Activity.label
             (Display.Activity.items (Display.activity display)));
        advance 2.0;
        Display.succeed copy;
        history := Display.history display)
  in
  let finished =
    List.filter_map
      (fun event ->
        match Display.Event.kind event with
        | `Finished outcome ->
            Some
              ( String.concat "/" (Display.Event.path event),
                match outcome with
                | `Succeeded -> "succeeded"
                | `Failed -> "failed"
                | `Cancelled -> "cancelled" )
        | _ -> None)
      !history
  in
  Alcotest.(check (list (pair string string)))
    "the group settles after its last row, with its own outcome"
    [
      ("service web/[1/2] FROM nginx", "succeeded");
      ("service web/[2/2] COPY site", "succeeded");
      ("service web", "failed");
    ]
    finished;
  let expected =
    [
      ("!! service web failed", "  3.5s");
      ("  => [1/2] FROM nginx", "  0.5s");
      ("  => [2/2] COPY site", "  3.0s");
    ]
  in
  let committed = committed_rows vte in
  Alcotest.(check int)
    "one block: the group and its two steps" (List.length expected)
    (List.length committed);
  List.iter2
    (fun (lead, timer) row ->
      Alcotest.(check bool)
        (lead ^ ": its outcome word, in its place in the block")
        true
        (String.starts_with ~prefix:lead row);
      Alcotest.(check string) (lead ^ ": its cost") timer (trailing 6 row))
    expected committed

let suite =
  ( "display",
    [
      Alcotest.test_case "a transient row leaves no line" `Quick
        test_transient_row_leaves_no_line;
      Alcotest.test_case "with_task owns outcome" `Quick
        test_with_task_owns_outcome;
      Alcotest.test_case "terminal operations are idempotent" `Quick
        test_terminal_operations_are_idempotent;
      Alcotest.test_case "parent settles children" `Quick
        test_parent_settles_children;
      Alcotest.test_case "failed parent cancels children" `Quick
        test_failed_parent_cancels_children;
      Alcotest.test_case "a swept row reaches no screen" `Quick
        test_a_swept_row_reaches_no_screen;
      Alcotest.test_case "task ownership" `Quick test_task_ownership;
      Alcotest.test_case "closed handles" `Quick test_closed_handles;
      Alcotest.test_case "unfinished auto-succeeded" `Quick
        test_unfinished_is_succeeded;
      Alcotest.test_case "exception cancellation" `Quick
        test_exception_cancels_and_propagates;
      Alcotest.test_case "validation" `Quick test_validation;
      Alcotest.test_case "nested run reuses owner" `Quick
        test_nested_run_reuses_owner;
      Alcotest.test_case "a group lays its rows beneath it at 80 columns" `Quick
        (test_group_lays_its_rows_beneath_it 80);
      Alcotest.test_case "a group lays its rows beneath it at 120 columns"
        `Quick
        (test_group_lays_its_rows_beneath_it 120);
      Alcotest.test_case "a group settles with its last row" `Quick
        test_group_settles_with_its_last_row;
    ] )
