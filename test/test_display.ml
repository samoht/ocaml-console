(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

exception Broken_formatter

let contains needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec loop i =
    i + n <= m && (String.sub haystack i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let count_occurrences needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec loop i count =
    if i + n > m then count
    else if String.sub haystack i n = needle then loop (i + n) (count + 1)
    else loop (i + 1) count
  in
  if n = 0 then 0 else loop 0 0

let fake_clock () =
  let time = ref 0. in
  ((fun () -> !time), fun dt -> time := !time +. dt)

let ctx ?(is_tty = false) ?(width = 80) ?(height = 24) ~ppf ~now () =
  Display.ctx ~ppf ~now ~dimensions:(fun () -> (width, height)) ~is_tty ()

let capture ?header ?bar ?theme f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let value =
    Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History ?header ?bar ?theme
      (fun display -> f display advance)
  in
  Format.pp_print_flush ppf ();
  (value, Buffer.contents buffer)

(* The same session on a live display. [capture] pins [`History], which is the
   appending writer; a terminal gets [`Activity], which paints a region and
   writes what is permanent around it. A result that reaches one and not the
   other is a result a developer does not get. *)
let capture_live ?header ?bar ?theme f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let value =
    Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ())
      ~mode:`Activity ?header ?bar ?theme (fun display -> f display advance)
  in
  Format.pp_print_flush ppf ();
  (value, Buffer.contents buffer)

let check_invalid message f =
  Alcotest.check_raises message (Invalid_argument message) f

let mib n = Int64.mul (Int64.of_int n) 1048576L
let gib n = Int64.mul (Int64.of_int n) 1073741824L

let test_bytes () =
  let check = Alcotest.(check string) in
  check "zero" "0 B" (Display.bytes 0L);
  check "small" "512 B" (Display.bytes 512L);
  check "kib" "1.50 KiB" (Display.bytes 1536L);
  check "mib" "30.00 MiB" (Display.bytes (mib 30));
  check "gib" "2.00 GiB" (Display.bytes (gib 2))

let test_span () =
  let check = Alcotest.(check string) in
  check "zero" "0.0s" (Display.span 0.);
  check "negative" "0.0s" (Display.span (-1.));
  check "tenths" "6.4s" (Display.span 6.44);
  check "under a minute" "59.9s" (Display.span 59.94);
  check "minutes" "1m05s" (Display.span 65.);
  check "hours" "1h02m" (Display.span 3725.)

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

(* A wrapper hands [run] a context and then has to write to the same surface
   itself: the newline that closes a line the session left open is written
   after the display is torn down, so it cannot come out of the display. The
   context is where that surface was named, and [ctx_ppf] is where the wrapper
   reads it, so the byte it writes lands in the session's own transcript and
   after it. *)
let test_a_context_names_the_surface_it_draws_on () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let ctx = ctx ~ppf ~now () in
  Display.run ~ctx ~mode:`History ~header:"Building" (fun display ->
      let resolve = Display.task display "resolve" in
      advance 1.3;
      Display.succeed resolve);
  Fmt.pf (Display.ctx_ppf ctx) "after the session@.";
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "the wrapper wrote onto the session's own surface, after it"
    ("=> resolve  1.3s\n" ^ "[+] Building 1.3s (1/1)\n" ^ "after the session\n")
    (Buffer.contents buffer)

(* Every log line and every finished row the display draws carries a marker,
   and the marker is the theme's. The default half pins the six default
   glyphs; the custom half is what proves they are read from the theme rather
   than recited beside it. *)
let markers_drawn ?theme () =
  let (), output =
    capture ?theme (fun display _ ->
        let row = Display.task display "note" in
        Display.log ~level:`Info row "an info line";
        Display.log ~level:`Warning row "a warning line";
        Display.log ~level:`Error row "an error line";
        Display.succeed row;
        Display.fail (Display.task display "broken");
        Display.cancel (Display.task display "abandoned"))
  in
  output

let test_markers_come_from_the_theme () =
  let defaults =
    [
      ("an info line", "# an info line");
      ("a warning line", "! a warning line");
      ("an error line", "x an error line");
      ("a succeeded row", "=> note");
      ("a failed row", "!! broken");
      ("a cancelled row", "-- abandoned");
    ]
  in
  let drawn = markers_drawn () in
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " keeps its glyph")
        true (contains line drawn))
    defaults;
  let theme =
    Theme.v ~note_marker:"(i)" ~warn_marker:"(w)" ~error_marker:"(e)"
      ~committed_succeeded:"(+)" ~committed_failed:"(-)"
      ~committed_cancelled:"(~)" ~spinner:Spinner.ascii ~done_marker:"[+]" ()
  in
  let drawn = markers_drawn ~theme () in
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " no longer draws the default")
        false (contains line drawn))
    defaults;
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " draws the theme's")
        true (contains line drawn))
    [
      ("an info line", "(i) an info line");
      ("a warning line", "(w) a warning line");
      ("an error line", "(e) an error line");
      ("a succeeded row", "(+) note");
      ("a failed row", "(-) broken");
      ("a cancelled row", "(~) abandoned");
    ]

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

let test_info_event_preserves_text () =
  let (), output =
    capture (fun display _ ->
        Display.event display "CONFLICT in lib/main.ml\n╭───╮\n│ x │\n╰───╯\n")
  in
  Alcotest.(check string)
    "script-compatible physical lines"
    "CONFLICT in lib/main.ml\n╭───╮\n│ x │\n╰───╯\n" output

let test_activity_snapshot () =
  let (before, after, final), _ =
    capture (fun display advance ->
        let parent = Display.task display "download" in
        let child = Display.task ~parent ~log_lines:2 display "layer" in
        Display.set_bytes child ~cur:4L ~total:10L;
        Display.log child "one";
        Display.log ~level:`Warning child "two";
        Display.log child "three";
        advance 2.;
        let before = Display.Activity.items (Display.activity display) in
        Display.succeed child;
        let after = Display.Activity.items (Display.activity display) in
        Display.succeed parent;
        (before, after, Display.activity display))
  in
  Alcotest.(check int) "two running" 2 (List.length before);
  let child = List.nth before 1 in
  Alcotest.(check string) "label" "layer" (Display.Activity.label child);
  let logs = Display.Activity.logs child in
  Alcotest.(check (list string))
    "bounded tail" [ "two"; "three" ]
    (List.map Display.Activity.log_message logs);
  Alcotest.(check bool)
    "severity retained" true
    (List.map Display.Activity.log_level logs = [ `Warning; `Info ]);
  Alcotest.(check bool)
    "bytes" true
    (Display.Activity.equal_progress
       (Display.Activity.progress child)
       (`Bytes (4L, 10L)));
  Alcotest.(check int) "finished child absent" 1 (List.length after);
  Alcotest.(check int)
    "nothing active after completion" 0
    (List.length (Display.Activity.items final))

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

let test_keyed_task () =
  let events, _ =
    capture (fun display _ ->
        let first = Display.task ~id:"compile:foo" display "waiting" in
        let second = Display.task ~id:"compile:foo" display "ignored" in
        Alcotest.(check bool) "stable handle" true (first == second);
        Display.set_label second "compile foo.ml";
        Display.set_count first ~cur:8 ~total:10;
        Display.set_count second ~cur:3 ~total:4;
        let item =
          List.hd (Display.Activity.items (Display.activity display))
        in
        Alcotest.(check bool)
          "monotonic progress" true
          (Display.Activity.equal_progress
             (Display.Activity.progress item)
             (`Count (8, 10)));
        Alcotest.(check (option string))
          "semantic key" (Some "compile:foo")
          (Display.Activity.key item);
        Display.succeed first;
        Display.history display)
  in
  Alcotest.(check bool)
    "one lifecycle" true
    (List.map Display.Event.kind events = [ `Started; `Finished `Succeeded ]);
  Alcotest.(check (list (option string)))
    "key retained"
    [ Some "compile:foo"; Some "compile:foo" ]
    (List.map Display.Event.key events)

let test_progress_estimates_expand () =
  let item, _ =
    capture (fun display _ ->
        Display.with_task ~total:8 display "split" (fun task ->
            Display.set_count task ~cur:22 ~total:8;
            Display.set_count task ~cur:23 ~total:874;
            Display.advance ~label:"writing objects" task;
            List.hd (Display.Activity.items (Display.activity display))))
  in
  Alcotest.(check string)
    "label updated atomically" "writing objects"
    (Display.Activity.label item);
  Alcotest.(check bool)
    "estimate grew and incremented" true
    (Display.Activity.equal_progress
       (Display.Activity.progress item)
       (`Count (24, 874)));
  let bytes_item, _ =
    capture (fun display _ ->
        Display.with_task display "download" (fun task ->
            Display.set_bytes task ~cur:22L ~total:8L;
            List.hd (Display.Activity.items (Display.activity display))))
  in
  Alcotest.(check bool)
    "byte estimate grew" true
    (Display.Activity.equal_progress
       (Display.Activity.progress bytes_item)
       (`Bytes (22L, 22L)))

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
         List.equal String.equal path_a path_b
         && Display.Event.equal_outcome outcome_a outcome_b)
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

(* An event stream carries one id across every scope its emitter passes
   through, so the same id arrives under a second parent as a matter of course.
   Offering one is a statement about where the work is, never a request to move
   a row that already exists: the first parent wins and nothing raises, because
   a display that can raise here can abort the operation it is drawing. *)
let test_key_keeps_its_first_parent () =
  let items =
    fst
    @@ capture (fun display _ ->
        let first = Display.task display "first" in
        let second = Display.task display "second" in
        let child = Display.task ~id:"child" ~parent:first display "child" in
        let again = Display.task ~id:"child" ~parent:second display "child" in
        Alcotest.(check bool)
          "the second offer returns the same task" true (again == child);
        List.map
          (fun item -> (Display.Activity.label item, item))
          (Display.Activity.items (Display.activity display)))
  in
  let item label =
    match List.assoc_opt label items with
    | Some item -> item
    | None -> Alcotest.failf "no %s row" label
  in
  Alcotest.(check (option int))
    "the child stays under the parent it started in"
    (Some (Display.Activity.id (item "first")))
    (Display.Activity.parent (item "child"));
  Alcotest.(check int) "no second row was created" 3 (List.length items)

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

let test_validation () =
  check_invalid "Console.Display.Line.bar: negative width" (fun () ->
      ignore (Display.Line.bar (-1)));
  check_invalid "Console.Display.Line.v: more than one label" (fun () ->
      ignore Display.Line.(v [ label; label ]));
  check_invalid "Console.Display.Line.v: more than one spacer" (fun () ->
      ignore Display.Line.(v [ spacer; spacer ]));
  check_invalid "Console.Display.run: width must be positive" (fun () ->
      Display.run ~ctx:(Display.dumb ()) ~width:0 ignore);
  check_invalid "Console.Display.run: height must be positive" (fun () ->
      Display.run ~ctx:(Display.dumb ()) ~height:0 ignore);
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

let test_failed_install_releases_owner () =
  let broken =
    Format.make_formatter
      (fun _buffer _offset _length -> raise Broken_formatter)
      (fun () -> raise Broken_formatter)
  in
  let now () = 0. in
  Alcotest.check_raises "formatter error is preserved" Broken_formatter
    (fun () ->
      Display.run
        ~ctx:(ctx ~is_tty:true ~ppf:broken ~now ())
        ~mode:`Activity ignore);
  let buffer = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buffer in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      let task = Display.task display "recovered" in
      Display.succeed task)

let hide = "\027[?25l"
let show = "\027[?25h"

(* A signal handler that ends the process is the one exit [run] never sees,
   so it asks the display for the terminal back before it re-raises. What the
   buffer holds at the call is the reading: the close after it shows the
   cursor again on its own. *)
let test_restore_terminal_from_a_handler () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let at_the_call = ref "" in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      let task = Display.task display "work" in
      Display.restore_terminal ();
      at_the_call := Buffer.contents buffer;
      Display.succeed task);
  Alcotest.(check bool)
    "the activity projection hid the cursor" true
    (contains hide !at_the_call);
  Alcotest.(check bool)
    "restore_terminal shows it, flushed, at the call" true
    (String.ends_with ~suffix:show !at_the_call);
  let (), history =
    capture (fun display _ ->
        let task = Display.task display "work" in
        Display.restore_terminal ();
        Display.succeed task)
  in
  Alcotest.(check bool)
    "a history display hid nothing, so nothing is shown" false
    (contains show history);
  (* Outside a run there is no terminal to give back, and no error either. *)
  Display.restore_terminal ()

let terminal_run ?(height = 8) ?(width = 40) ?header f =
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  let feed () =
    Format.pp_print_flush ppf ();
    Vte.feed_string vte (Buffer.contents buffer);
    Buffer.clear buffer
  in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height ~ppf ~now ())
    ~mode:`Activity ~width ~height ?header (fun display ->
      (* What a reader sees: the driver's tick has painted the snapshot. *)
      let sync () =
        Display.tick display;
        feed ()
      in
      f display advance sync vte;
      sync ());
  feed ();
  vte

let terminal_rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

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
   left. On a pipe nothing between the reports is drawn at all.

   MUTATION: a touch that paints on every report reds this on the count of
   frames. *)
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
      let last = Display.bytes total ^ " / " in
      Alcotest.(check bool)
        "a report inside the interval is not yet on screen" false
        (contains last (Buffer.contents buffer));
      Display.tick display;
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the tick draws it" true
        (contains last (Buffer.contents buffer));
      Display.succeed task)

let test_differential_redraw_and_right_edge () =
  let width = 48 and height = 8 in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let vte = Vte.create ~rows:height ~cols:width () in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height ~ppf ~now ())
    ~mode:`Activity ~width ~height (fun display ->
      ignore (Display.task display "[R1] prepare");
      let changed = Display.task display "[R2] download" in
      ignore (Display.task display "[R3] verify");
      Display.tick display;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buffer);
      Buffer.clear buffer;
      let initial = Vte.to_string vte |> String.split_on_char '\n' in
      let first = List.nth initial 0 and last = List.nth initial 2 in
      for cur = 1 to 10 do
        Vte.clear_dirty vte;
        Display.set_count changed ~cur ~total:10;
        Display.tick display;
        Format.pp_print_flush ppf ();
        Vte.feed_string vte (Buffer.contents buffer);
        Buffer.clear buffer;
        Alcotest.(check (list int))
          "only the changed terminal row is repainted" [ 1 ]
          (Vte.dirty_rows vte);
        let rows = Vte.to_string vte |> String.split_on_char '\n' in
        Alcotest.(check string)
          "preceding sibling is stable" first (List.nth rows 0);
        Alcotest.(check string)
          "following sibling is stable" last (List.nth rows 2);
        let row = List.nth rows 1 in
        Alcotest.(check int)
          "elapsed reaches right edge" width (Width.string_width row);
        Alcotest.(check bool)
          "right edge is metadata" true
          (String.ends_with ~suffix:"0.0s" row)
      done;
      Alcotest.(check int)
        "right-edge redraw never scrolls" 0 (Vte.scrollback_size vte);
      Alcotest.(check (pair int int))
        "cursor rests on the final activity row" (2, 0) (Vte.cursor_pos vte))

let test_zero_progress_has_no_empty_bar () =
  ignore
    (terminal_run ~height:4 ~width:56 (fun display _advance sync vte ->
         let task = Display.task display "checking packages" in
         Display.set_count task ~cur:0 ~total:459;
         sync ();
         let row = List.hd (String.split_on_char '\n' (Vte.to_string vte)) in
         Alcotest.(check bool)
           "zero progress has no orphaned delimiters" false
           (contains "▕" row || contains "▏" row);
         Alcotest.(check bool)
           "count remains informative" true (contains "0/459" row);
         Display.set_count task ~cur:1 ~total:459;
         sync ();
         let row = List.hd (String.split_on_char '\n' (Vte.to_string vte)) in
         Alcotest.(check bool)
           "first completed item has visible fill" true (contains "▕▏" row)))

let test_committed_metadata_is_right_aligned () =
  let width = 48 in
  let vte =
    terminal_run ~height:8 ~width (fun display advance _sync _vte ->
        let finish ?count elapsed label =
          let task = Display.task display label in
          Option.iter
            (fun (cur, total) -> Display.set_count task ~cur ~total)
            count;
          advance elapsed;
          Display.succeed task
        in
        finish ~count:(1, 2) 0.3 "pins synced";
        finish ~count:(459, 459) 1.1 "packages checked";
        finish 0.1 "workspace prepared")
  in
  let rows =
    terminal_rows vte
    |> List.filter (fun row -> String.starts_with ~prefix:"=>" row)
  in
  Alcotest.(check int) "three permanent task rows" 3 (List.length rows);
  List.iter
    (fun row ->
      Alcotest.(check int)
        "committed row reaches the right edge" width (Width.string_width row);
      Alcotest.(check bool)
        "elapsed time is the rightmost field" true
        (String.ends_with ~suffix:"s" row))
    rows;
  Alcotest.(check int)
    "committed rows do not enter scrollback" 0 (Vte.scrollback_size vte);
  Alcotest.(check (pair int int))
    "cursor begins the row after history" (3, 0) (Vte.cursor_pos vte)

(* The trailing cells of a row as the terminal received them: its last
   [columns] columns, which are the count cell, two spaces and the elapsed
   cell. Bytes are columns here -- a bar glyph or a wide label sits further
   left, and a count pair and a span are ASCII. *)
let trailing columns row =
  let n = String.length row in
  if n < columns then row else String.sub row (n - columns) columns

(* A whole block of rows shares one column grid: a count cell sized from the
   widest total the block carries ("470/470", seven columns), two spaces, and a
   timer cell six columns wide. So a row counting to 2 puts its pair in the same
   columns as one counting to 470, a row with no count leaves those columns
   empty rather than sliding its timer left, and the row whose timer passed ten
   seconds does not sit a column off its neighbours. The expected cells are the
   grid itself, not this display's own answer read back. *)
let test_rows_share_one_grid () =
  let width = 70 in
  let vte =
    terminal_run ~height:10 ~width (fun display advance _sync _vte ->
        let finish ?count elapsed label =
          let task = Display.task display label in
          Option.iter
            (fun (cur, total) -> Display.set_count task ~cur ~total)
            count;
          advance elapsed;
          Display.succeed task
        in
        finish ~count:(12, 256) 0.0 "pins synced";
        finish ~count:(470, 470) 0.2 "opam synced";
        finish ~count:(470, 470) 11.4 "packages checked";
        finish 0.4 "workspace prepared";
        finish 0.0 "split";
        finish ~count:(2, 2) 1.0 "pushed")
  in
  let rows =
    terminal_rows vte
    |> List.filter (fun row -> String.starts_with ~prefix:"=>" row)
  in
  let expected =
    [
      (" 12/256", "  0.0s");
      ("470/470", "  0.2s");
      ("470/470", " 11.4s");
      ("       ", "  0.4s");
      ("       ", "  0.0s");
      ("    2/2", "  1.0s");
    ]
  in
  Alcotest.(check int)
    "six permanent task rows" (List.length expected) (List.length rows);
  List.iter2
    (fun (count, elapsed) row ->
      Alcotest.(check int)
        "committed row reaches the right edge" width (Width.string_width row);
      Alcotest.(check string)
        "count cell, two spaces, timer cell"
        (count ^ "  " ^ elapsed)
        (trailing 15 row))
    expected rows;
  Alcotest.(check int)
    "committed rows do not enter scrollback" 0 (Vte.scrollback_size vte)

(* The same grid on a row that is still running. The timer crosses ten seconds
   and then a minute; it grows inside its own cell, so the count to its left
   holds its columns and the live line does not jump as it ticks. *)
let test_timer_keeps_its_cell () =
  let width = 70 in
  let seen = ref [] in
  ignore
    (terminal_run ~height:6 ~width (fun display advance sync vte ->
         let task = Display.task display "packages checked" in
         Display.set_count task ~cur:470 ~total:470;
         List.iter
           (fun dt ->
             advance dt;
             sync ();
             let row =
               List.hd (String.split_on_char '\n' (Vte.to_string vte))
             in
             seen := !seen @ [ trailing 15 row ])
           [ 9.9; 0.1; 90.0 ];
         Display.succeed task));
  Alcotest.(check (list string))
    "the timer grows inside its cell"
    [ "470/470    9.9s"; "470/470   10.0s"; "470/470   1m40s" ]
    !seen

(* The count cell is the block's and is sized from each row's total, so the
   columns a live label may take are one budget for the whole block: a row that
   has counted nothing yet towards 470 and a row with no count at all both give
   the label what is left after a seven-column count cell ("470/470"), not after
   the five their own text needs ("0/470") or the none a countless row needs. On
   a forty-column terminal that leaves 21 columns (a spinner, a space, the
   spacer's column, the count cell, two separators and the six-column timer
   cell), so a 22-column label loses its last word on both rows. A row
   counting from zero draws no bar, so the bar takes no columns from either. *)
let test_block_shares_one_label_budget () =
  let width = 40 in
  let rows = ref [] in
  ignore
    (terminal_run ~height:6 ~width (fun display _advance sync vte ->
         let counting = Display.task display "workspace prepared now" in
         Display.set_count counting ~cur:0 ~total:470;
         let bare = Display.task display "packages checked twice" in
         sync ();
         rows :=
           List.filteri
             (fun i _ -> i < 2)
             (String.split_on_char '\n' (Vte.to_string vte));
         Display.succeed counting;
         Display.succeed bare));
  let expected =
    [
      ("workspace prepared now", "workspace prepared", "  0/470    0.0s");
      ("packages checked twice", "packages checked", "           0.0s");
    ]
  in
  Alcotest.(check int) "two live rows" 2 (List.length !rows);
  List.iter2
    (fun (label, kept, cells) row ->
      Alcotest.(check bool)
        (label ^ " is shortened to the block's budget")
        false (contains label row);
      Alcotest.(check bool)
        (label ^ " keeps its leading words whole")
        true
        (contains (" " ^ kept ^ " ") row);
      Alcotest.(check bool)
        (label ^ " carries no ellipsis")
        false
        (contains "\xe2\x80\xa6" row);
      Alcotest.(check string)
        (label ^ ": count cell, two spaces, timer cell")
        cells (trailing 15 row))
    expected !rows

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

let test_default_visual_hierarchy_and_color_scope () =
  let width = 60 in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  (* The accent exists because this formatter renders style; the terminal alone
     does not decide it. *)
  Fmt.set_style_renderer ppf `Ansi_tty;
  let now () = 0. in
  let vte = Vte.create ~rows:4 ~cols:width () in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height:4 ~ppf ~now ())
    ~mode:`Activity ~width ~height:4 (fun display ->
      let root = Display.task display "compile" in
      ignore (Display.task ~parent:root display "link");
      Display.tick display;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buffer);
      let rows = Vte.to_string vte |> String.split_on_char '\n' in
      Alcotest.(check bool)
        "root starts with the status glyph, not a fake tree branch" true
        (String.starts_with ~prefix:"⠋ compile" (List.nth rows 0));
      Alcotest.(check bool)
        "only nested work gets a guide" true
        (String.starts_with ~prefix:"  └── ⠋ link" (List.nth rows 1));
      let grid = Vte.grid vte in
      let is_accent x =
        let i = Grid.idx grid ~x ~y:0 in
        Grid.get_fg_r grid i < 0.5
        && Grid.get_fg_g grid i > 0.5
        && Grid.get_fg_b grid i > 0.5
      in
      let accented =
        List.init width Fun.id |> List.filter is_accent |> List.length
      in
      Alcotest.(check int)
        "accent is scoped to the live status glyph" 1 accented)

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

(* A row is drawn once and so is its output. [log] commits a line permanently
   above the activity region and also keeps it in the row's bounded live tail,
   which the next paint draws under the spinner, so every log line an apply
   emits is on screen twice at once, printed once in history and again below
   the row it belongs to. The live tail is where a line lives while its row
   runs; permanent history is where it goes when it ages out of that tail or
   the row finishes. *)
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

(* A cut reads as a wrap. Truncation adds no marker, so a row whose label does
   not fit simply ends at the column the terminal ends at -- in the middle of a
   hex digest, indistinguishable from a value continuing on the next line. The
   cut is marked, and marked in the middle: the leading characters identify a
   digest and the trailing ones name a file, so a cut that keeps only one end
   throws away the half the reader needed. *)
let digest_label =
  "sha256:a376ef543f48c9b17ed2b1cbe32b4d1e6a5f0c9d8e7b6a5948372615f0e1d2c3"

(* A row that does not fit the terminal is shortened by words, never by
   characters: it drops words the row can lose, or folds onto a second line
   the way git and cargo do. A path, an id or a digest is always printed
   whole. The row below is a digest between two words, on a terminal
   too narrow for all of it. *)
let pushed_label = "pushed " ^ digest_label ^ " to the registry"

let check_whole what ~width rows =
  Alcotest.(check bool)
    (what ^ " prints the digest whole")
    true
    (List.exists (contains digest_label) rows);
  Alcotest.(check bool)
    (what ^ " carries no ellipsis")
    false
    (List.exists (contains "\xe2\x80\xa6") rows);
  List.iter
    (fun row ->
      Alcotest.(check bool)
        (what ^ " fits the terminal")
        true
        (Width.string_width row <= width))
    rows

let test_row_keeps_a_digest_whole () =
  let width = 100 in
  let committed =
    terminal_run ~height:6 ~width (fun display _advance sync vte ->
        let task = Display.task display pushed_label in
        sync ();
        check_whole "the live row" ~width
          [ List.hd (String.split_on_char '\n' (Vte.to_string vte)) ];
        Display.succeed task)
  in
  let rows = terminal_rows committed in
  check_whole "the committed row" ~width rows;
  Alcotest.(check bool)
    "the word that did not fit beside it folds under it" true
    (List.exists (fun row -> String.equal (String.trim row) "registry") rows)

(* A row's label is the display's own naming of a value, and the cut above is
   right for it. A log line is not: it is the caller's own text, printed by the
   program the developer wrote, and a cut through its middle joins two ends that
   were never next to each other. What comes out reads as a shorter sentence
   rather than as a line that was shortened, which is what stops a reader
   noticing that anything is gone.

   The line below is a payload service's own output, taken from a run of the
   three-service exemplar on a 100-column terminal. Its own docstring sends the
   developer to watch the frames it drops and the cloud threshold it drops them
   over, and a middle cut removes exactly that clause and leaves a sentence
   behind. So a log line keeps its start and loses words from its end, whole:
   no word is cut and no mark is drawn. *)
let service_line =
  "[filter] kept 76 of 96 frames, 466944 bytes, dropped 20 over 0.35 cloud \
   (from the recorded pass in scene/, not from camera: services cannot send \
   frames to each other yet)"

let ellipsis = "\xe2\x80\xa6"

let rstrip row =
  let n = ref (String.length row) in
  while !n > 0 && row.[!n - 1] = ' ' do
    decr n
  done;
  String.sub row 0 !n

let index_of needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec loop i =
    if i + n > m then None
    else if String.sub haystack i n = needle then Some i
    else loop (i + 1)
  in
  loop 0

(* The live row, read while the task still runs: history commits a log line
   whole, so a screen read after the row settles would answer about the
   permanent copy and not about the one being fitted. *)
let logged_line ~width =
  let drawn = ref None in
  ignore
    (terminal_run ~height:8 ~width (fun display _advance sync vte ->
         let task = Display.task ~log_lines:2 display "payload/filter" in
         Display.log task service_line;
         sync ();
         drawn :=
           List.find_opt (contains "kept 76")
             (String.split_on_char '\n' (Vte.to_string vte));
         Display.succeed task));
  match !drawn with
  | None -> Alcotest.fail "the service's own line never reached the screen"
  | Some row -> rstrip row

let check_word_cut what ~width row =
  Alcotest.(check bool)
    (what ^ " fits the terminal")
    true
    (Width.string_width row <= width);
  Alcotest.(check bool)
    (what ^ " carries no ellipsis")
    false (contains ellipsis row);
  match index_of "[filter]" row with
  | None -> Alcotest.fail (what ^ " lost the start of the service's line")
  | Some start ->
      let message = String.sub row start (String.length row - start) in
      let n = String.length message in
      Alcotest.(check string)
        (what ^ " is the service's own line, from its start")
        (String.sub service_line 0 (min n (String.length service_line)))
        message;
      Alcotest.(check bool)
        (what ^ " ends where a word of the line ends")
        true
        (n >= String.length service_line || service_line.[n] = ' ')

let test_a_log_line_loses_words_from_its_end () =
  let row = logged_line ~width:100 in
  check_word_cut "a service's line on a 100-column terminal" ~width:100 row;
  Alcotest.(check bool)
    "the number the developer is told to watch is kept" true
    (contains "dropped 20 over 0.35 cloud" row)

let test_a_narrow_log_line_loses_words_from_its_end () =
  check_word_cut "a service's line on a 60-column terminal" ~width:60
    (logged_line ~width:60)

(* Every label and line this suite draws, on every terminal from 20 to 200
   columns wide: the live frame and the history after it carry no U+2026. *)
let ellipsis_fixtures =
  [
    digest_label;
    pushed_label;
    service_line;
    "workspace prepared now";
    "packages checked twice";
    "a very long task label with \xe4\xbd\xa0\xe5\xa5\xbd and control\tbytes";
    "Target: waiting for op-hello's boot verdict (300s bound)";
  ]

let test_no_row_carries_an_ellipsis () =
  for width = 20 to 200 do
    List.iter
      (fun label ->
        let live = ref "" in
        let vte =
          terminal_run ~height:12 ~width ~header:label
            (fun display _advance sync vte ->
              let task = Display.task ~log_lines:2 display label in
              Display.log task service_line;
              Display.log task label;
              sync ();
              live := Vte.to_string vte;
              Display.succeed task)
        in
        List.iter
          (fun row ->
            if contains ellipsis row then
              Alcotest.failf "%d columns, %S: a row carries U+2026: %S" width
                label row)
          (String.split_on_char '\n' !live @ terminal_rows vte))
      ellipsis_fixtures;
    List.iter
      (fun line ->
        if contains ellipsis (Span.to_string line) then
          Alcotest.failf "%d columns: Span.wrap marks a cut: %S" width
            (Span.to_string line))
      (Span.wrap width (Span.text digest_label))
  done

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

let test_large_percentage_does_not_overflow () =
  let count_ok = ref false and bytes_ok = ref false in
  let bar =
    Display.Line.(
      v [ label; spacer; percent; text " "; count; text " "; elapsed ])
  in
  let _vte =
    terminal_run ~height:5 ~width:72 (fun display _ sync vte ->
        let count = Display.task ~bar display "count" in
        Display.set_count count ~cur:max_int ~total:max_int;
        sync ();
        count_ok := contains "100%" (Vte.to_string vte);
        Display.succeed count;
        let bytes = Display.task ~bar display "bytes" in
        Display.set_bytes bytes ~cur:Int64.max_int ~total:Int64.max_int;
        sync ();
        bytes_ok := contains "100%" (Vte.to_string vte);
        Display.succeed bytes)
  in
  Alcotest.(check bool) "max_int percentage" true !count_ok;
  Alcotest.(check bool) "int64 percentage" true !bytes_ok

(* The SGR sequences in [s]: a CSI whose final byte is ['m']. A cursor move
   (final ['A']), an erase (['K']) and the cursor-visibility pair (['h'], ['l'])
   are CSIs too and are not counted, which is what separates no colour from no
   redraw. *)
let sgr_sequences s =
  let n = String.length s in
  let rec final j =
    if j >= n then j
    else
      let c = Char.code s.[j] in
      if c >= 0x40 && c <= 0x7e then j else final (j + 1)
  in
  let rec go i count =
    if i + 1 >= n then count
    else if s.[i] = '\027' && s.[i + 1] = '[' then
      let stop = final (i + 2) in
      go (stop + 1) (if stop < n && s.[stop] = 'm' then count + 1 else count)
    else go (i + 1) count
  in
  go 0 0

let colour_width = 40
let colour_height = 10

(* One live session on a terminal of a fixed geometry, drawn on a formatter
   whose style renderer is [renderer]: a row that logs and finishes, then a
   verdict. Its bytes are what the terminal received. *)
let colour_session renderer =
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf renderer;
  let now () = 0. in
  Display.run
    ~ctx:
      (ctx ~is_tty:true ~width:colour_width ~height:colour_height ~ppf ~now ())
    ~mode:`Activity ~width:colour_width ~height:colour_height ~header:"Build"
    (fun display ->
      let task = Display.task display "app" in
      Display.log task "planning";
      Display.succeed task;
      Display.set_result display (`Ok "done"));
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let colour_screen bytes =
  let vte = Vte.create ~rows:colour_height ~cols:colour_width () in
  Vte.feed_string vte bytes;
  vte

(* The screen the session leaves: the log line the finished row handed to
   history, the row's own committed line with its elapsed time at the right
   edge, then the header and the verdict the close writes. Four rows, whatever
   the colour: the region was redrawn in place rather than appended to. *)
let colour_expected_screen =
  [
    "  # planning";
    "=> app" ^ String.make 30 ' ' ^ "0.0s";
    "[+] Build 0.0s (1/1)";
    "✓ done";
  ]

(* The known-correct screen: the same four lines typed straight at a terminal
   of the same geometry, with no styling and no cursor control to reach a
   frame above. Comparing the session's screen against this one, cell for
   cell, says both halves at once -- a cell that carries a style the plain
   text does not, and a glyph that is not where the plain text put it. *)
let colour_plain_screen () =
  let vte = Vte.create ~rows:colour_height ~cols:colour_width () in
  List.iter
    (fun line -> Vte.feed_string vte (line ^ "\r\n"))
    colour_expected_screen;
  vte

let unlike_plain_text vte =
  let drawn = Vte.grid vte and plain = Vte.grid (colour_plain_screen ()) in
  List.filter
    (fun i -> not (Grid.cells_equal drawn i plain i))
    (List.init (Grid.width plain * Grid.height plain) Fun.id)

(* A reader who turned colour off still gets the redraw: no SGR reaches the
   terminal, and the screen is the one the same text typed plainly leaves --
   no cell styled, every glyph where the plain text put it, so the region was
   redrawn in place rather than appended to. https://no-color.org asks that
   "the addition of ANSI color" be prevented, and the library adds it through
   one answer -- [Fmt.style_renderer] on the formatter it writes to -- which
   is the answer every other surface here already reads. *)
let test_a_plain_renderer_writes_no_sgr () =
  let bytes = colour_session `None in
  Alcotest.(check int) "no SGR reached the terminal" 0 (sgr_sequences bytes);
  let vte = colour_screen bytes in
  Alcotest.(check (list string))
    "the region still redrew in place" colour_expected_screen
    (Console_vte.screen ~scrollback:true vte);
  Alcotest.(check int)
    "no cell differs from the same text typed plainly" 0
    (List.length (unlike_plain_text vte))

(* The positive control, and the reading that says the session had colour to
   lose: the same session on a formatter that renders style carries the
   outcome's green, the label's bold and the metadata's dim, and puts exactly
   the same glyphs on the screen. *)
let test_an_ansi_renderer_writes_sgr () =
  let bytes = colour_session `Ansi_tty in
  Alcotest.(check bool)
    "the committed marker is green" true
    (contains "\027[32m" bytes);
  Alcotest.(check bool) "the label is bold" true (contains "\027[1m" bytes);
  Alcotest.(check bool) "the metadata is dim" true (contains "\027[2m" bytes);
  Alcotest.(check (list string))
    "colour changes no glyph" colour_expected_screen
    (Console_vte.screen ~scrollback:true (colour_screen bytes))

(* No colour is not no redraw. The cursor control the live region is made of
   is emitted whatever the style renderer says: it hides the cursor, moves up
   over the frame it is replacing, erases to the end of a line it shortened,
   and shows the cursor again at the close. *)
let test_a_plain_renderer_keeps_the_cursor_control () =
  let bytes = colour_session `None in
  let has what sequence =
    Alcotest.(check bool) what true (contains sequence bytes)
  in
  has "the cursor was hidden" "\027[?25l";
  has "the frame above was reached" "\027[1A";
  has "a shortened line was erased" "\027[K";
  has "the cursor came back" "\027[?25h"

(* The append-only layout is compact, not colourless: a history-mode session on
   a formatter that renders style carries the outcome's green on a committed
   row's marker and dim on its metadata, and its header carries the same styles
   the terminal header does. The one answer whether to colour is the
   formatter's own style renderer, so the same session on [`None] adds no SGR
   at all and the glyphs are unchanged. *)
let history_colour_session renderer =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf renderer;
  let now, advance = fake_clock () in
  Display.run ~ctx:(ctx ~ppf ~now ()) ~mode:`History ~header:"Build"
    (fun display ->
      let task = Display.task display "app" in
      advance 1.3;
      Display.succeed task);
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

(* The glyphs and layout of [s], with every SGR sequence removed. *)
let strip_sgr s =
  let n = String.length s in
  let buffer = Buffer.create n in
  let rec final j =
    if j >= n then j
    else
      let c = Char.code s.[j] in
      if c >= 0x40 && c <= 0x7e then j else final (j + 1)
  in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && s.[i] = '\027' && s.[i + 1] = '[' then begin
      let stop = final (i + 2) in
      if stop < n && s.[stop] = 'm' then go (stop + 1)
      else begin
        Buffer.add_substring buffer s i (stop + 1 - i);
        go (stop + 1)
      end
    end
    else begin
      Buffer.add_char buffer s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buffer

let test_history_carries_colour_on_a_styled_surface () =
  let bytes = history_colour_session `Ansi_tty in
  Alcotest.(check bool)
    "the committed marker carries the outcome's green" true
    (contains "\027[1m\027[32m=>\027[0m app" bytes);
  Alcotest.(check bool)
    "the committed metadata is dim" true
    (contains "\027[2m  1.3s\027[0m" bytes)

let test_history_adds_no_sgr_on_a_plain_surface () =
  Alcotest.(check int)
    "no SGR reached the pipe" 0
    (sgr_sequences (history_colour_session `None))

let test_history_glyphs_do_not_depend_on_colour () =
  Alcotest.(check string)
    "colour changes no glyph"
    (history_colour_session `None)
    (strip_sgr (history_colour_session `Ansi_tty))

let suite =
  ( "display",
    [
      Alcotest.test_case "bytes" `Quick test_bytes;
      Alcotest.test_case "span" `Quick test_span;
      Alcotest.test_case "history output" `Quick test_history_output;
      Alcotest.test_case "a context names the surface it draws on" `Quick
        test_a_context_names_the_surface_it_draws_on;
      Alcotest.test_case "a transient row leaves no line" `Quick
        test_transient_row_leaves_no_line;
      Alcotest.test_case "markers come from the theme" `Quick
        test_markers_come_from_the_theme;
      Alcotest.test_case "event history" `Quick test_event_history;
      Alcotest.test_case "info event preserves text" `Quick
        test_info_event_preserves_text;
      Alcotest.test_case "activity snapshot" `Quick test_activity_snapshot;
      Alcotest.test_case "logs remain history" `Quick test_logs_are_not_activity;
      Alcotest.test_case "late logs remain history" `Quick
        test_late_log_is_history;
      Alcotest.test_case "result" `Quick test_result;
      Alcotest.test_case "result on a live display" `Quick test_result_live;
      Alcotest.test_case "keyed task" `Quick test_keyed_task;
      Alcotest.test_case "progress estimates expand" `Quick
        test_progress_estimates_expand;
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
      Alcotest.test_case "key keeps its first parent" `Quick
        test_key_keeps_its_first_parent;
      Alcotest.test_case "task ownership" `Quick test_task_ownership;
      Alcotest.test_case "closed handles" `Quick test_closed_handles;
      Alcotest.test_case "unfinished auto-succeeded" `Quick
        test_unfinished_is_succeeded;
      Alcotest.test_case "exception cancellation" `Quick
        test_exception_cancels_and_propagates;
      Alcotest.test_case "first result wins" `Quick test_result_first_wins;
      Alcotest.test_case "validation" `Quick test_validation;
      Alcotest.test_case "control sanitization" `Quick test_sanitizes_controls;
      Alcotest.test_case "reporter scope" `Quick test_reporter_scope;
      Alcotest.test_case "nested run reuses owner" `Quick
        test_nested_run_reuses_owner;
      Alcotest.test_case "failed install releases owner" `Quick
        test_failed_install_releases_owner;
      Alcotest.test_case "restore_terminal from a handler" `Quick
        test_restore_terminal_from_a_handler;
      Alcotest.test_case "activity tick does not leak" `Quick
        test_activity_does_not_leak_on_ticks;
      Alcotest.test_case "reports between ticks paint at the eye's pace" `Quick
        test_reports_between_ticks_paint_at_the_eyes_pace;
      Alcotest.test_case "differential redraw and right edge" `Quick
        test_differential_redraw_and_right_edge;
      Alcotest.test_case "zero progress has no empty bar" `Quick
        test_zero_progress_has_no_empty_bar;
      Alcotest.test_case "committed metadata is right aligned" `Quick
        test_committed_metadata_is_right_aligned;
      Alcotest.test_case "a block shares one column grid" `Quick
        test_rows_share_one_grid;
      Alcotest.test_case "a ticking timer keeps its cell" `Quick
        test_timer_keeps_its_cell;
      Alcotest.test_case "a live block shares one label budget" `Quick
        test_block_shares_one_label_budget;
      Alcotest.test_case "a group lays its rows beneath it at 80 columns" `Quick
        (test_group_lays_its_rows_beneath_it 80);
      Alcotest.test_case "a group lays its rows beneath it at 120 columns"
        `Quick
        (test_group_lays_its_rows_beneath_it 120);
      Alcotest.test_case "a group settles with its last row" `Quick
        test_group_settles_with_its_last_row;
      Alcotest.test_case "default visual hierarchy and color scope" `Quick
        test_default_visual_hierarchy_and_color_scope;
      Alcotest.test_case "activity collapse" `Quick test_activity_collapse;
      Alcotest.test_case "history survives activity" `Quick
        test_history_survives_activity;
      Alcotest.test_case "a log line is drawn once" `Quick
        test_log_reaches_the_terminal_once;
      Alcotest.test_case "a row keeps a digest whole" `Quick
        test_row_keeps_a_digest_whole;
      Alcotest.test_case "no row carries an ellipsis" `Quick
        test_no_row_carries_an_ellipsis;
      Alcotest.test_case "a log line loses words from its end" `Quick
        test_a_log_line_loses_words_from_its_end;
      Alcotest.test_case "a narrow log line loses words from its end" `Quick
        test_a_narrow_log_line_loses_words_from_its_end;
      Alcotest.test_case "a window line never reaches history" `Quick
        test_a_window_line_never_reaches_history;
      Alcotest.test_case "an appending display writes no window" `Quick
        test_an_appending_display_writes_no_window;
      Alcotest.test_case "activity lines bounded" `Quick
        test_activity_lines_are_bounded;
      Alcotest.test_case "tiny activity bounded" `Quick
        test_tiny_activity_is_bounded;
      Alcotest.test_case "large percentage" `Quick
        test_large_percentage_does_not_overflow;
      Alcotest.test_case "a plain renderer writes no SGR" `Quick
        test_a_plain_renderer_writes_no_sgr;
      Alcotest.test_case "an ansi renderer writes SGR" `Quick
        test_an_ansi_renderer_writes_sgr;
      Alcotest.test_case "a plain renderer keeps the cursor control" `Quick
        test_a_plain_renderer_keeps_the_cursor_control;
      Alcotest.test_case "history carries colour on a styled surface" `Quick
        test_history_carries_colour_on_a_styled_surface;
      Alcotest.test_case "history adds no SGR on a plain surface" `Quick
        test_history_adds_no_sgr_on_a_plain_surface;
      Alcotest.test_case "history glyphs do not depend on colour" `Quick
        test_history_glyphs_do_not_depend_on_colour;
    ] )
