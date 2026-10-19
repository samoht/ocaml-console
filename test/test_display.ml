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
  Display.run ~ctx:(ctx ~ppf ~now ()) (fun first ->
      let parent = Display.task first "first" in
      foreign := Some parent;
      Display.succeed parent);
  Display.run ~ctx:(ctx ~ppf ~now ()) (fun second ->
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
  Display.run ~ctx:(ctx ~ppf ~now ()) (fun display ->
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
      Display.run ~ctx:(ctx ~ppf ~now ()) (fun display ->
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
  capture (fun display _ ->
      check_invalid "Console.Display.task: negative log_lines" (fun () ->
          ignore (Display.task ~log_lines:(-1) display "bad"));
      let task = Display.task display "bounded" in
      check_invalid "Console.Display.set_count: expected non-negative progress"
        (fun () -> Display.set_count task ~cur:(-1) ~total:1);
      check_invalid "Console.Display.set_bytes: expected non-negative progress"
        (fun () -> Display.set_bytes task ~cur:(-1L) ~total:1L);
      Display.set_count task ~cur:1 ~total:2;
      check_invalid "Console.Display.set_bytes: task already tracks count"
        (fun () -> Display.set_bytes task ~cur:1L ~total:2L);
      Display.set_result display (`Ok "settled"))
  |> ignore

(* One display runs at a time: a second one, whatever its mode, is refused,
   and the first one's ending frees the claim. *)
let test_one_display_at_a_time () =
  let buffer = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let c = ctx ~live:true ~ppf ~now () in
  let refused live =
    match Display.run ~ctx:(ctx ~live ~ppf ~now ()) ignore with
    | () -> false
    | exception Invalid_argument _ -> true
  in
  Display.run ~ctx:c (fun _ ->
      Alcotest.(check bool) "a second live display" true (refused true);
      Alcotest.(check bool) "a second appending display" true (refused false));
  Alcotest.(check bool) "free once the first ends" false (refused false)

(* A display draws its colours at the depth its context names: the amber of
   [Theme.dos] is a 24-bit colour, drawn as its nearest palette entry. *)
let test_colours_follow_the_depth () =
  let drawn depth =
    let buffer = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buffer in
    Fmt.set_style_renderer ppf `Ansi_tty;
    let ctx =
      Display.ctx ~depth ~ppf
        ~now:(fun () -> 0.)
        ~wait:ignore
        ~dimensions:(fun () -> (40, 10))
        ~live:true ()
    in
    Display.run ~ctx ~theme:Theme.dos ~header:"Building" (fun display ->
        ignore (Display.task display "compile");
        Display.tick display);
    Format.pp_print_flush ppf ();
    Buffer.contents buffer
  in
  Alcotest.(check bool)
    "24-bit colour as given" true
    (contains "38;2;255;176;0" (drawn `True_color));
  Alcotest.(check bool)
    "a palette entry at 256 colours" false
    (contains "38;2;" (drawn `Ansi_256))

let suite =
  ( "display",
    [
      Alcotest.test_case "colours follow the depth" `Quick
        test_colours_follow_the_depth;
      Alcotest.test_case "a transient row leaves no line" `Quick
        test_transient_row_leaves_no_line;
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
      Alcotest.test_case "one display at a time" `Quick
        test_one_display_at_a_time;
    ] )
