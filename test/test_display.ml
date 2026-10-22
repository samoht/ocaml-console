(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let strings = Alcotest.(list string)
let check_lines msg expected actual = Alcotest.check strings msg expected actual
let theme = Theme.dos

(* [untimed l] is [l] without the timer at its end. *)
let untimed = Re.replace_string (Re.Perl.compile_pat " +[0-9.]+s$") ~by:""

(* [above top] is the view of [top] above the default view, which gives way
   first when they do not fit. *)
let above top d ~width ~height =
  Span.vcat [ top; Display.default_view d ~width ~height ]

(* {1 Without a terminal} *)

let test_plain_lines () =
  let (), out =
    plain ~theme ~summary:"Building" (fun d advance ->
        let t = Display.task d "compile" in
        Display.set_count t ~cur:2 ~total:2;
        Display.log t "compiled parser.ml";
        advance 1.5;
        Display.log t "slow";
        Display.succeed t;
        Driver.report d `Info "done")
  in
  check_lines "lines in turn, the header last"
    [
      "[-] compile";
      "  # compile: compiled parser.ml";
      "  # compile: slow  +1.5s";
      "[+] compile  2/2  1.5s";
      "done";
      "[+] Building 1.5s (1/1)";
      "";
    ]
    (String.split_on_char '\n' out)

(* A line the display prints starts at the first column, even after the
   program left a line unfinished on the same formatter. *)
let test_plain_column () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Display.v ~now:(fun () -> 0.) ppf in
  Fmt.pf ppf "partial%!";
  Driver.report d `Info "next";
  Fmt.pf ppf "again";
  Display.succeed (Display.task d "t");
  Display.close d;
  Fmt.pf ppf "after@.";
  check_lines "each display line on its own"
    [ "partial"; "next"; "again"; "[-] t"; "[+] t  0.0s"; "after"; "" ]
    (String.split_on_char '\n' (Buffer.contents buffer))

(* A task still running when the display closes did not finish: it is
   cancelled, unless the program said the work succeeded. *)
let test_plain_unfinished () =
  let (), out = plain ~theme (fun d _ -> ignore (Display.task d "left")) in
  check_lines "cancelled"
    [ "[-] left"; "[~] left  0.0s  cancelled"; "" ]
    (String.split_on_char '\n' out);
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Display.task d "left");
        Display.set_result d (Ok "done"))
  in
  check_lines "succeeded with the work"
    [ "[-] left"; "[+] left  0.0s"; "[+] done"; "" ]
    (String.split_on_char '\n' out)

(* Without a terminal no colour may tell a failure from a success: the
   closing line of a task that did not succeed says how it ended, in the
   muted style of its time. *)
let test_plain_outcome_word () =
  let (), out =
    plain ~theme (fun d _ ->
        Display.succeed (Display.task d "ok");
        let a = Display.task d "a" in
        Display.set_count a ~cur:2 ~total:2;
        Display.fail a;
        Display.cancel (Display.task d "b"))
  in
  check_lines "the outcome after the time"
    [
      "[-] ok";
      "[+] ok  0.0s";
      "[-] a";
      "[x] a  2/2  0.0s  failed";
      "[-] b";
      "[~] b  0.0s  cancelled";
      "";
    ]
    (String.split_on_char '\n' out);
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `Ansi_tty;
  let d = Display.v ~now:(fun () -> 0.) ppf in
  Display.fail (Display.task d "a");
  Display.close d;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "muted with the time" true
    (contains "0.0s  failed\027[" (Buffer.contents buffer))

let test_plain_attribution () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Display.task d "a" and b = Display.task d "b" in
        Display.log a "one";
        Display.log b "two";
        Display.log a "three";
        Display.fail a;
        Display.cancel b)
  in
  check_lines "each line says its task, in order"
    [
      "[-] a";
      "[-] b";
      "  # a: one";
      "  # b: two";
      "  # a: three";
      "[x] a  0.0s  failed";
      "[~] b  0.0s  cancelled";
      "";
    ]
    (String.split_on_char '\n' out)

(* A tab in a task's output goes to the next stop of eight columns from the
   start of its line, so tabbed columns stay aligned. *)
let test_tabs () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Display.task d "a" in
        Display.log a "x\ty\n\u{754c}\tz\n12345678\tw";
        Display.fail a)
  in
  check_lines "tab stops"
    [
      "[-] a";
      "  # a: x       y";
      "  # a: \u{754c}      z";
      "  # a: 12345678        w";
      "[x] a  0.0s  failed";
      "";
    ]
    (String.split_on_char '\n' out)

let test_plain_sanitizes () =
  let (), out =
    plain ~summary:"Build\027[2J" (fun d _ ->
        let task = Display.task d "a\tb" in
        Display.log task "x\007y";
        Display.succeed task)
  in
  Alcotest.(check bool) "no escape" false (String.contains out '\027');
  Alcotest.(check bool) "log text" true (contains "x y" out)

let test_plain_result () =
  let (), out =
    plain ~theme ~summary:"Work" (fun d _ ->
        ignore (Display.task d "left running");
        Display.set_result ~detail:"see the log" d (Error "it broke");
        Display.set_result d (Ok "changed my mind"))
  in
  check_lines "running tasks cancelled, verdict last"
    [
      "[-] left running";
      "[~] left running  0.0s  cancelled";
      "[x] Work 0.0s (1/1)";
      "see the log";
      "[x] it broke";
      "";
    ]
    (String.split_on_char '\n' out)

let test_empty_header_pending () =
  let t = terminal ~summary:"Building" () in
  Alcotest.(check bool)
    "pending before any task" true
    (List.exists (contains "\xe2\x80\xa2 Building 0.0s (0/0)") (screen t));
  Display.close t.display;
  Alcotest.(check bool)
    "succeeded once closed" true
    (List.exists (contains "\xe2\x9c\x93 Building 0.0s (0/0)") (lines t))

(* {1 On a terminal} *)

let test_region () =
  let t = terminal ~theme ~width:40 ~summary:"Build" () in
  let a = Display.task t.display "fetch" in
  let b = Display.task t.display "compile" in
  Display.set_count b ~cur:1 ~total:4;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool) "first row" true (contains "fetch" (List.nth rows 0));
  Alcotest.(check bool) "second row" true (contains "compile" (List.nth rows 1));
  Alcotest.(check bool) "its count" true (contains "1/4" (List.nth rows 1));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows 2));
  Display.succeed a;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the ended task is above the region" true
    (String.starts_with ~prefix:"[+] fetch" (List.nth rows 0));
  Alcotest.(check bool)
    "the summary still last" true
    (contains "[-] Build" (List.nth rows 2));
  Display.succeed b;
  Display.close t.display;
  check_lines "final screen"
    [
      "[+] fetch                           0.0s";
      "[+] compile                  1/4    0.0s";
      "[+] Build 0.0s (2/2)";
    ]
    (lines t);
  Alcotest.(check bool) "cursor shown" true (contains "\027[?25h" (written t));
  Alcotest.(check bool)
    "no bare line feed" false
    (Re.execp (Re.Perl.compile_pat "(^|[^\r])\n") (written t))

(* A view with a table taller than the room: the table loses its first
   lines, and the tasks and the summary stay. *)
let test_tall_header () =
  let t = terminal ~width:40 ~height:8 ~summary:"S" () in
  let d = t.display in
  let rows =
    Span.vcat (List.init 20 (fun i -> Span.text (Fmt.str "row %d" i)))
  in
  Display.set_view d (above rows);
  let a = Display.task d "a" in
  Display.redraw d;
  let shown = List.map untimed (screen t) in
  Alcotest.(check int) "the screen less one" 7 (List.length shown);
  check_lines "the table's last lines"
    [ "row 15"; "row 16"; "row 17"; "row 18"; "row 19" ]
    (List.filteri (fun i _ -> i < 5) shown);
  Alcotest.(check bool) "the task" true (contains " a" (List.nth shown 5));
  Alcotest.(check bool) "the summary" true (contains "S" (List.nth shown 6));
  Display.succeed a;
  Display.close d

(* With the tty in raw mode a line feed keeps the column, so a line that
   began with a bare line feed would start where the one above it ended: the
   screen of a raw tty is that of a cooked one. *)
let test_raw_tty () =
  let run raw =
    let t = terminal ~theme ~width:40 ~summary:"Build" ~raw () in
    let a = Display.task t.display "fetch" in
    let b = Display.task t.display "compile" in
    Display.set_count b ~cur:1 ~total:4;
    Display.redraw t.display;
    let live = screen t in
    Display.succeed a;
    Driver.report t.display `Info "note";
    Display.set_count b ~cur:2 ~total:4;
    Display.redraw t.display;
    let later = screen t in
    Display.succeed b;
    Display.close t.display;
    (live, later, lines t)
  in
  let live, later, final = run false in
  let live', later', final' = run true in
  check_lines "the region" live live';
  check_lines "the region after a line above it" later later';
  check_lines "the final screen" final final'

(* The host left "Fetching " unended. The region starts on the next row,
   so its first line, nearly as wide as the screen, does not wrap there and
   a repaint erases all of it. *)
let test_after_a_partial_line () =
  let run before =
    let t =
      terminal ~theme ~width:40 ~before ~summary:(String.make 28 'h') ()
    in
    Display.redraw t.display;
    let a = Display.task t.display "fetch" in
    Display.redraw t.display;
    let first = screen t in
    Display.succeed a;
    Display.close t.display;
    (first, lines t)
  in
  let first, final = run "" in
  let first', final' = run "Fetching " in
  let used rows = List.rev (List.drop_while (( = ) "") (List.rev rows)) in
  check_lines "the region below the host's line" ("Fetching" :: used first)
    (used first');
  check_lines "the final screen" ("Fetching" :: final) final'

(* Rule 1: while a task runs, its window shows its latest lines of both
   kinds, a line too wide cut there and only there. *)
let test_window () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let wide = "wide " ^ String.make 40 'w' in
  Display.with_task ~log_lines:3 t.display "job" (fun job ->
      List.iter (Display.log job) [ "one"; "two" ];
      Driver.report t.display `Info "note";
      Display.log job wide;
      Display.redraw t.display;
      let rows = screen t in
      Alcotest.(check bool) "the row" true (contains "job" (List.nth rows 0));
      check_lines "the latest three lines"
        [ "  # two"; "  note"; "  # " ^ String.sub wide 0 25 ^ "\u{2026}" ]
        (List.filteri (fun i _ -> i > 0) rows));
  Display.close t.display;
  check_lines "a success folds them all away"
    [ "[+] job                   0.0s" ]
    (lines t)

let test_alignment () =
  let t = terminal ~theme ~width:40 () in
  let a = Display.task t.display "\xe7\x95\x8c\xe7\x95\x8c wide" in
  let b =
    Display.task t.display "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9 emoji"
  in
  let c = Display.task t.display "plain" in
  Display.set_count a ~cur:1 ~total:10;
  Display.set_count b ~cur:10 ~total:100;
  Display.set_count c ~cur:3 ~total:3;
  List.iter Display.succeed [ a; b; c ];
  Display.close t.display;
  let rows = lines t in
  let col needle row =
    let rec find i =
      if i + String.length needle > String.length row then -1
      else if String.sub row i (String.length needle) = needle then i
      else find (i + 1)
    in
    let byte = find 0 in
    Span.width (Span.text (String.sub row 0 byte))
  in
  Alcotest.(check (list int))
    "counts end in one column" [ 32; 32; 32 ]
    (List.map2
       (fun n r -> col n r + String.length n)
       [ "1/10"; "10/100"; "3/3" ]
       rows);
  Alcotest.(check (list int))
    "timers end at the edge" [ 40; 40; 40 ]
    (List.map (fun r -> col "0.0s" r + 4) rows)

let test_label_truncated () =
  let t = terminal ~theme ~width:20 () in
  ignore (Display.task t.display "a label much too long for the row");
  Display.redraw t.display;
  let row = List.nth (screen t) 0 in
  Alcotest.(check bool) "ellipsis" true (contains "\xe2\x80\xa6" row);
  Alcotest.(check bool) "fits" true (Span.width (Span.text row) <= 20)

(* More running tasks than rows: the latest show, after a line that counts
   the others, and the summary stays last. No redraw scrolls the terminal. *)
let test_capped () =
  let t = terminal ~theme ~width:30 ~height:5 ~summary:"H" () in
  let tasks =
    List.init 8 (fun i ->
        let task = Display.task t.display (Fmt.str "task %d" i) in
        Display.redraw t.display;
        task)
  in
  Display.redraw t.display;
  let rows = List.filter (( <> ) "") (screen t) in
  check_lines "no taller than the screen less one"
    [ "  \u{2026} 6 more running"; "| task 6"; "| task 7"; "[-] H 0.0s (0/8)" ]
    (List.map untimed rows);
  Alcotest.(check int) "nothing scrolled" 0 (Console_vte.scrollback_size t.vte);
  List.iter Display.cancel tasks;
  Display.close t.display

(* Eight running tasks with a log line each fill a budget of eight lines
   exactly: the windows give way and every task shows. Two rows fewer, and
   only the latest tasks show, a line counting the others. *)
let test_marker_counts () =
  let run height =
    let t = terminal ~theme ~width:30 ~height ~summary:"H" () in
    let tasks =
      List.init 8 (fun i ->
          let task =
            Display.task ~log_lines:1 t.display (Fmt.str "task %d" i)
          in
          Display.log task "line";
          task)
    in
    Display.redraw t.display;
    let rows = List.filter (( <> ) "") (screen t) in
    List.iter Display.cancel tasks;
    Display.close t.display;
    List.map untimed rows
  in
  check_lines "the windows give way"
    (List.init 8 (Fmt.str "| task %d") @ [ "[-] H 0.0s (0/8)" ])
    (run 10);
  check_lines "the latest tasks"
    [
      "  \u{2026} 3 more running";
      "| task 3";
      "| task 4";
      "| task 5";
      "| task 6";
      "| task 7";
      "[-] H 0.0s (0/8)";
    ]
    (run 8)

let test_marker_fits () =
  let t = terminal ~theme ~width:12 ~height:4 () in
  let tasks = List.init 6 (fun i -> Display.task t.display (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  (* A row wider than the terminal would wrap onto a fourth. *)
  Alcotest.(check int)
    "three rows" 3
    (List.length (List.filter (( <> ) "") (screen t)));
  List.iter Display.cancel tasks;
  Display.close t.display

let test_with_output () =
  let t = terminal ~theme ~width:30 () in
  let a = Display.task t.display "job" in
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "hello@.");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check string) "printed above" "hello" (List.nth rows 0);
  Alcotest.(check bool) "region below" true (contains "job" (List.nth rows 1));
  (match Display.with_output t.display (fun _ -> failwith "boom") with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  Display.redraw t.display;
  Alcotest.(check bool)
    "region drawn again after a raise" true
    (List.exists (contains "job") (screen t));
  Display.succeed a;
  Display.close t.display

(* Output without a line end is ended before the region is drawn again. *)
let test_with_output_partial () =
  let t = terminal ~theme ~width:30 () in
  let a = Display.task t.display "job" in
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "partial");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check string) "its own line" "partial" (List.nth rows 0);
  Alcotest.(check bool) "region below" true (contains "job" (List.nth rows 1));
  Display.succeed a;
  Display.close t.display

(* Bytes of the program's that move the cursor act above the region, and
   the region is drawn whole below them, on rows it erases first. The tty
   is raw, so that a line feed does not return the cursor either. *)
let test_with_output_moves () =
  List.iter
    (fun move ->
      let t = terminal ~theme ~raw:true ~width:30 () in
      let a = Display.task t.display "job" in
      Display.with_output t.display (fun ppf ->
          Fmt.pf ppf "ZZZZZZZZZZ%sZZZZZZZZZZ@." move);
      Display.redraw t.display;
      let row = List.find (contains "job") (screen t) in
      Alcotest.(check bool)
        (Fmt.str "%S: the row is its own: %S" move row)
        true
        (String.starts_with ~prefix:"| job" row && not (String.contains row 'Z'));
      Display.succeed a;
      Display.close t.display)
    [ "\027[A"; "\027[2A\r"; "\r"; "\027[K"; "\027[H" ]

let test_redraw_idle () =
  let t = terminal ~theme () in
  ignore (Display.task t.display "job");
  Display.redraw t.display;
  let before = List.length !(t.chunks) in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "nothing written" before (List.length !(t.chunks));
  Display.close t.display

(* A terminal that narrows reflows the old frame onto more rows; the redraw
   moves up over all of them. *)
let test_narrowing () =
  let t = terminal ~theme ~width:40 ~height:8 ~summary:"Header" () in
  ignore (Display.task t.display "job");
  Display.redraw t.display;
  t.chunks := [];
  resize t ~width:20 ~height:8;
  Display.redraw t.display;
  (* Two lines of 40 cells take four rows at 20. *)
  Alcotest.(check bool)
    "moved up over the reflowed rows" true
    (contains "\r\027[3A\027[J" (written t));
  Display.close t.display

let test_height_changes () =
  let t = terminal ~theme ~width:30 ~height:4 () in
  let tasks = List.init 5 (fun i -> Display.task t.display (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  resize t ~width:30 ~height:10;
  Display.redraw t.display;
  resize t ~width:30 ~height:3;
  Display.redraw t.display;
  Alcotest.(check bool)
    "no line wider than the terminal" true
    (List.for_all (fun r -> Span.width (Span.text r) <= 30) (screen t));
  List.iter Display.succeed tasks;
  Display.close t.display

let test_nested () =
  let t = terminal ~width:30 ~height:10 () in
  let p = Display.task t.display "parent" in
  let c1 = Display.task ~parent:p t.display "one" in
  let _c2 = Display.task ~parent:p t.display "two" in
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "branch" true
    (contains "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80" (List.nth rows 1));
  Alcotest.(check bool)
    "last" true
    (contains "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80" (List.nth rows 2));
  Display.succeed c1;
  Display.redraw t.display;
  Alcotest.(check bool)
    "an ended child stays under its parent" true
    (contains "\u{2713} one" (List.nth (screen t) 1));
  Display.fail p;
  let late = Display.task ~parent:p t.display "late" in
  Display.succeed late;
  Display.close t.display;
  check_lines "the parent first, each child under it, in start order"
    [
      "\u{2717} parent";
      "  \u{251c}\u{2500}\u{2500} \u{2713} one";
      "  \u{2514}\u{2500}\u{2500} \u{25cb} two";
      "\u{25cb} late";
    ]
    (List.map untimed (lines t))

(* A task that its parent ends still says how it ended, on a pipe too. *)
let test_nested_plain () =
  let (), out =
    plain ~theme (fun d _ ->
        let p = Display.task d "parent" in
        let c1 = Display.task ~parent:p d "one" in
        ignore (Display.task ~parent:p d "two");
        Display.succeed c1;
        Display.fail p)
  in
  check_lines "a start line and an end line each"
    [
      "[-] parent";
      "[-] parent \u{203A} one";
      "[-] parent \u{203A} two";
      "[+] parent \u{203A} one  0.0s";
      "[~] parent \u{203A} two  0.0s  cancelled";
      "[x] parent  0.0s  failed";
      "";
    ]
    (String.split_on_char '\n' out)

let test_keys_and_transient () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Display.task ~key:"k" d "first" in
        let b = Display.task ~key:"k" d "second" in
        Alcotest.(check bool) "same task" true (a == b);
        Display.succeed a;
        let c = Display.task ~key:"k" d "third" in
        Alcotest.(check bool) "a new task once it ended" false (a == c);
        Display.succeed c;
        Display.succeed (Display.task ~transient:true d "gone"))
  in
  check_lines "no line for a transient task"
    [ "[-] first"; "[+] first  0.0s"; "[-] third"; "[+] third  0.0s"; "" ]
    (String.split_on_char '\n' out)

(* A with_task that finds its key running enters that task and ends
   nothing: the task ends as the call that started it says. *)
let test_with_task_shared_key () =
  let (), out =
    plain ~theme (fun d _ ->
        Display.with_task ~key:"k" d "first" (fun a ->
            Display.with_task ~key:"k" d "second" (fun b ->
                Alcotest.(check bool) "same task" true (a == b);
                Display.log b "inside");
            Display.log a "after the second";
            match
              Display.with_task ~key:"k" d "third" (fun _ -> raise Exit)
            with
            | () -> Alcotest.fail "the exception was swallowed"
            | exception Exit -> Display.log a "after the third"))
  in
  check_lines "one task, ended once by the first"
    [
      "[-] first";
      "  # first: inside";
      "  # first: after the second";
      "  # first: after the third";
      "[+] first  0.0s";
      "";
    ]
    (String.split_on_char '\n' out)

let test_counts () =
  let (), out =
    plain ~theme (fun d _ ->
        let t = Display.task d "count" in
        Display.set_count t ~cur:5 ~total:10;
        Display.set_count t ~cur:3 ~total:8;
        Display.set_count t ~cur:12 ~total:10;
        Display.succeed t;
        let b = Display.task d "bytes" in
        Display.set_bytes b ~cur:1048576L ~total:2097152L;
        check_invalid "Console.Display.set_count: the task counts bytes"
          (fun () -> Display.set_count b ~cur:1 ~total:1);
        Display.succeed b)
  in
  check_lines "never back, total raised"
    [
      "[-] count";
      "[+] count  12/12  0.0s";
      "[-] bytes";
      "[+] bytes  1.00 MiB / 2.00 MiB  0.0s";
      "";
    ]
    (String.split_on_char '\n' out)

(* A count near [max_int] still shows its share. *)
let test_large_counts () =
  let layout = Display.Layout.(v [ label; text " "; percent ]) in
  let t = terminal ~theme ~layout ~width:30 () in
  let task = Display.task t.display "big" in
  Display.set_count task ~cur:(max_int / 2) ~total:max_int;
  Display.redraw t.display;
  Alcotest.(check bool)
    "half" true
    (contains " 49%" (List.nth (screen t) 0)
    || contains " 50%" (List.nth (screen t) 0));
  Display.succeed task;
  Display.close t.display

let test_invalid () =
  let d =
    Display.v ~now:(fun () -> 0.) (Format.formatter_of_buffer (Buffer.create 1))
  in
  let other =
    Display.v ~now:(fun () -> 0.) (Format.formatter_of_buffer (Buffer.create 1))
  in
  check_invalid "Console.Display.task: empty key" (fun () ->
      ignore (Display.task ~key:"" d "x"));
  check_invalid "Console.Display.task: negative log_lines" (fun () ->
      ignore (Display.task ~log_lines:(-1) d "x"));
  check_invalid "Console.Display.task: parent belongs to another display"
    (fun () -> ignore (Display.task ~parent:(Display.task other "p") d "x"));
  let t = Display.task d "t" in
  check_invalid "Console.Display.set_count: negative progress" (fun () ->
      Display.set_count t ~cur:(-1) ~total:1);
  check_invalid "Console.Display.Layout.bar: negative width" (fun () ->
      ignore (Display.Layout.bar (-1)));
  check_invalid "Console.Display.Layout.v: more than one label" (fun () ->
      ignore Display.Layout.(v [ label; label ]));
  Display.close d

(* [size] has no documented bound, and a terminal reports what it is told
   ($COLUMNS under [`Tty] without a terminal): a display on a terminal of
   any width runs, and writes in proportion to what it draws, not to the
   width. *)
let test_any_width () =
  List.iter
    (fun w ->
      let buf = Buffer.create 256 in
      let ppf = Format.formatter_of_buffer buf in
      match
        let d = Display.v ~size:(fun () -> (w, 24)) ~now:(fun () -> 0.) ppf in
        Display.with_task d "work" (fun t ->
            Display.set_count t ~cur:1 ~total:2);
        Display.close d
      with
      | () ->
          Format.pp_print_flush ppf ();
          if Buffer.length buf > 100_000 then
            Alcotest.failf "width %d: %d bytes written" w (Buffer.length buf)
      | exception e -> Alcotest.failf "width %d: %s" w (Printexc.to_string e))
    [ 1_000_000; Sys.max_string_length; max_int ]

(* A fiber that outlives the display, or a library that holds on to it,
   is ignored once it closed: nothing raises and nothing is written. *)
let test_closed () =
  let t = terminal ~theme ~summary:"Build" () in
  let d = t.display in
  let running = Display.task d "running" in
  Display.close d;
  let before = written t in
  Display.close d;
  let late = Display.task d "late" in
  List.iter
    (fun task ->
      Display.set_label task "renamed";
      Display.set_count task ~cur:1 ~total:2;
      Display.log task "late line";
      Display.succeed task;
      Display.fail task;
      Display.cancel task)
    [ running; late ];
  Display.redraw d;
  Display.set_view d (fun _ ~width:_ ~height:_ -> Span.text "view");
  Display.set_result d (Ok "late");
  Alcotest.(check string) "nothing written" before (written t);
  Alcotest.(check int)
    "with_output still calls its function" 42
    (Display.with_output d (fun _ -> 42))

let test_colours () =
  let t = terminal ~styled:true () in
  let a = Display.task t.display "job" in
  Display.fail a;
  Display.close t.display;
  let out = written t in
  (* The default theme's red is entry 160 of the 256-colour palette. *)
  Alcotest.(check bool) "red failure" true (contains "\027[1;38;5;160m" out);
  Alcotest.(check bool) "styles closed" true (contains "\027[0m" out)

(* On a terminal wider than 100 columns, the rows and the lines of ended
   tasks keep to 100, so that a count and a timer stay near their label. *)
let test_wide () =
  let t = terminal ~width:160 ~height:8 () in
  let task = Display.task t.display "compile" in
  Display.set_count task ~cur:1 ~total:3;
  Display.log task "a log line";
  Display.redraw t.display;
  let widths () = List.map Width.string_width (lines t) in
  Alcotest.(check int) "the live row" 100 (List.fold_left max 0 (widths ()));
  Display.succeed task;
  Display.close t.display;
  Alcotest.(check int)
    "the line it ends on" 100
    (List.fold_left max 0 (widths ()))

(* On a terminal, each write of the display is one synchronized update, so
   that a terminal that knows the mode shows a frame whole; a pipe gets no
   such escape. *)
let test_synchronized () =
  let begin_ = "\027[?2026h" and end_ = "\027[?2026l" in
  let t = terminal ~summary:"Build" () in
  let task = Display.task t.display "a" in
  t.advance 0.2;
  Display.redraw t.display;
  Display.log task "x";
  Display.succeed task;
  Display.close t.display;
  let occurrences sub s = List.length (Astring.String.cuts ~sep:sub s) - 1 in
  List.iter
    (fun c ->
      if
        not
          (String.starts_with ~prefix:begin_ c
          && String.ends_with ~suffix:end_ c
          && occurrences begin_ c = 1
          && occurrences end_ c = 1)
      then Alcotest.failf "not one synchronized update: %S" c)
    !(t.chunks);
  let (), out = plain (fun d _ -> Display.succeed (Display.task d "a")) in
  Alcotest.(check bool) "none on a pipe" false (contains "?2026" out)

(* {1 Task output and Logs messages} *)

let split out = String.split_on_char '\n' out

(* Rules 1 and 5: on a pipe there is no window: a task says when it starts,
   each of its lines comes at once after its label, and it says when it
   ends. *)
let test_pipe_lines () =
  let (), out =
    plain ~theme (fun d _ ->
        Display.with_task d "job" (fun job ->
            Display.log job "one";
            Driver.report d `Info "note";
            Display.log job "two"))
  in
  check_lines "in turn, each after its task's label"
    [
      "[-] job";
      "  # job: one";
      "  job: note";
      "  # job: two";
      "[+] job  0.0s";
      "";
    ]
    (split out)

(* Rule 5: the start line is written when the task starts, so a task that
   hangs leaves a trace. *)
let test_pipe_start () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Display.v ~now:(fun () -> 0.) ppf in
  let p = Display.task d "fetch" in
  ignore (Display.task ~parent:p d "index");
  Format.pp_print_flush ppf ();
  check_lines "the start lines, at once"
    [ "[-] fetch"; "[-] fetch \u{203A} index"; "" ]
    (split (Buffer.contents buffer));
  Display.close d

(* A task that succeeds drops its output, on a terminal, even the
   lines that left its window. *)
let test_success_drops () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let job = Display.task ~log_lines:2 t.display "job" in
  List.iter (Display.log job) [ "one"; "two"; "three" ];
  Display.redraw t.display;
  Alcotest.(check bool)
    "not above while it runs" false
    (List.exists (contains "one") (screen t));
  Display.succeed job;
  Display.close t.display;
  check_lines "only its line" [ "[+] job                   0.0s" ] (lines t)

(* A task that fails prints all its output under its line, in one
   block, without its label and not cut: a line too wide goes on over the
   rows below. [with_task] fails its task when its function raises. *)
let test_failure_block () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let wide = String.make 45 'w' in
  (match
     Display.with_task ~log_lines:2 t.display "job" (fun job ->
         List.iter (Display.log job) [ "l1"; "l2"; "l3"; wide ];
         failwith "boom")
   with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  Display.close t.display;
  let rows = lines t in
  Alcotest.(check string) "the failure first" "[x] job" (untimed (List.hd rows));
  check_lines "then the block"
    [ "  # l1"; "  # l2"; "  # l3" ]
    (List.filteri (fun i _ -> i >= 1 && i <= 3) rows);
  let rest = List.filteri (fun i _ -> i > 3) rows in
  Alcotest.(check string)
    "the wide line whole" ("  # " ^ wide)
    (String.concat ""
       (List.mapi
          (fun i l -> if i = 0 then l else String.sub l 4 (String.length l - 4))
          rest));
  Alcotest.(check bool) "no label" false (List.exists (contains "job:") rows)

(* The block holds at most 1 MiB, the latest lines, and one line
   says how many were dropped. *)
let test_failure_cap () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let job = Display.task t.display "job" in
  let n = 1200 in
  let line i = Fmt.str "line%04d%s" i (String.make 992 'x') in
  for i = 0 to n - 1 do
    Display.log job (line i)
  done;
  t.chunks := [];
  Display.fail job;
  let out = written t in
  let tokens =
    Re.all (Re.Perl.compile_pat "line([0-9]{4})") out
    |> List.map (fun g -> int_of_string (Re.Group.get g 1))
  in
  let first = List.fold_left min n tokens in
  check_lines "one line per line kept"
    (List.init (n - first) (fun i -> Fmt.str "%d" (first + i)))
    (List.map string_of_int tokens);
  let size i j =
    List.fold_left ( + ) 0
      (List.init (j - i) (fun k -> String.length (line (i + k)) + 1))
  in
  Alcotest.(check bool) "at most 1 MiB" true (size first n <= 1048576);
  Alcotest.(check bool) "as much as fits" true (size (first - 1) n > 1048576);
  Alcotest.(check bool)
    "how many were dropped" true
    (contains (Fmt.str "%d lines dropped" first) out);
  Display.close t.display

(* On a pipe nothing is kept: every line was printed when it came, and
   none is printed again under the failure. *)
let test_failure_plain () =
  let n = 2000 in
  let (), out =
    plain ~theme (fun d _ ->
        let job = Display.task ~log_lines:2 d "job" in
        for i = 0 to n - 1 do
          Display.log job (Fmt.str "line%04d %s" i (String.make 600 'x'))
        done;
        Display.fail job)
  in
  let lines = split out in
  Alcotest.(check int)
    "every line once, and the task's two lines"
    (n + 2 + 1)
    (List.length lines);
  let tail = List.filteri (fun i _ -> i >= n + 1) lines in
  check_lines "nothing again under the failure"
    [ "[x] job  0.0s  failed"; "" ]
    tail

(* A task that is cancelled, as Ctrl-C cancels them all, prints all its
   output under its line, as a failure does, the lines that left its window
   too. *)
let test_cancelled_output () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let job = Display.task ~log_lines:2 t.display "job" in
  let other = Display.task ~log_lines:1 t.display "other" in
  List.iter (Display.log job) [ "a"; "b"; "c" ];
  Display.log other "z";
  Display.cancel job;
  Display.close t.display;
  check_lines "all of it under each line"
    [ "[~] job"; "  # a"; "  # b"; "  # c"; "[~] other"; "  # z" ]
    (List.map untimed (lines t));
  let (), out =
    plain ~theme (fun d _ ->
        let job = Display.task ~log_lines:2 d "job" in
        Display.log job "a";
        Display.cancel job)
  in
  check_lines "nothing repeated on a pipe"
    [ "[-] job"; "  # job: a"; "[~] job  0.0s  cancelled"; "" ]
    (split out)

(* [root] is the display's top task: within it, output goes above the rows
   at once and stays, while a task runs and when that task fails, and a task
   started there has no parent. It never ends. *)
let test_root () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let d = t.display in
  let root = Display.root d in
  (match
     Display.with_task ~log_lines:2 d "job" (fun job ->
         Display.log job "mine";
         Display.within root (fun () -> Driver.report d `Warning "above");
         Display.log root "also above";
         Display.redraw d;
         check_lines "above the rows at once"
           [ "! above"; "also above" ]
           (List.filteri (fun i _ -> i < 2) (screen t));
         Alcotest.(check bool)
           "back in the task" true
           (match Driver.current d with Some c -> c == job | None -> false);
         failwith "boom")
   with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  Display.succeed root;
  Display.fail root;
  Display.cancel root;
  Display.log root "still there";
  Display.close d;
  check_lines "they stay, the task's own under its failure"
    [ "! above"; "also above"; "[x] job"; "  # mine"; "still there" ]
    (List.map untimed (lines t));
  let (), out =
    plain ~theme (fun d _ ->
        Display.with_task d "job" (fun _ ->
            Display.within (Display.root d) (fun () ->
                Driver.report d `Info "above";
                Display.succeed (Display.task d "top"));
            Driver.report d `Info "inside"))
  in
  check_lines "on a pipe, unlabelled"
    [
      "[-] job";
      "above";
      "[-] top";
      "[+] top  0.0s";
      "  job: inside";
      "[+] job  0.0s";
      "";
    ]
    (split out)

(* [within task f] makes a task that runs current for [f], and ends nothing:
   what [f] prints and the tasks it starts are [task]'s, also when [f]
   raises. *)
let test_within () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Display.task d "a" in
        Display.with_task d "b" (fun _ ->
            let v =
              Display.within a (fun () ->
                  Driver.report d `Info "for a";
                  ignore (Display.task d "under a");
                  7)
            in
            Alcotest.(check int) "its value" 7 v;
            Driver.report d `Info "for b");
        (match Display.within a (fun () -> failwith "inside") with
        | () -> Alcotest.fail "no exception"
        | exception Failure _ -> ());
        Display.log a "still runs";
        Display.succeed a)
  in
  check_lines "the lines of the task given"
    [
      "[-] a";
      "[-] b";
      "  a: for a";
      "[-] a \u{203A} under a";
      "  b: for b";
      "[+] b  0.0s";
      "  # a: still runs";
      "[+] a \u{203A} under a  0.0s";
      "[+] a  0.0s";
      "";
    ]
    (split out)

(* Two tasks that run at once: on a terminal each block is printed under
   its own line, never interleaved with the other's. *)
let test_two_tasks () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let a = Display.task t.display "a" and b = Display.task t.display "b" in
  List.iter
    (fun (task, s) -> Display.log task s)
    [ (a, "a1"); (b, "b1"); (a, "a2"); (b, "b2") ];
  Display.fail a;
  Display.fail b;
  Display.close t.display;
  check_lines "one block each"
    [ "[x] a"; "  # a1"; "  # a2"; "[x] b"; "  # b1"; "  # b2" ]
    (List.map untimed (lines t))

(* [with_task]: the task is current for its function, so tasks and Logs
   messages inside it go under it; it succeeds when the function returns,
   unless the function ended it first. *)
let test_with_task () =
  let v, out =
    plain ~theme (fun d _ ->
        Display.with_task d "outer" (fun _ ->
            Display.with_task d "inner" (fun _ -> Driver.report d `Info "deep");
            ignore (Display.task d "child");
            Display.with_task d "ended" (fun t ->
                Display.fail t;
                Driver.report d `Info "after its end";
                Display.log t "ignored");
            42))
  in
  Alcotest.(check int) "its value" 42 v;
  check_lines "nested under the current task"
    [
      "[-] outer";
      "[-] outer \u{203A} inner";
      "  outer \u{203A} inner: deep";
      "[+] outer \u{203A} inner  0.0s";
      "[-] outer \u{203A} child";
      "[-] outer \u{203A} ended";
      "[x] outer \u{203A} ended  0.0s  failed";
      "  outer \u{203A} ended: after its end";
      "[+] outer \u{203A} child  0.0s";
      "[+] outer  0.0s";
      "";
    ]
    (split out)

(* [iter] is a task whose count follows the list. *)
let test_iter () =
  let seen = ref [] in
  let (), out =
    plain ~theme (fun d _ ->
        Display.iter d "items" (fun x -> seen := x :: !seen) [ 1; 2; 3 ])
  in
  Alcotest.(check (list int)) "in order" [ 1; 2; 3 ] (List.rev !seen);
  check_lines "counted" [ "[-] items"; "[+] items  3/3  0.0s"; "" ] (split out)

(* {1 Views} *)

let steps =
  Table.v
    [
      Table.column (Span.text "step");
      Table.column ~align:`Right (Span.text "time");
    ]
    [
      [ Span.text "fetch"; Span.text "1.2s" ];
      [ Span.text "compile"; Span.text "3.4s" ];
    ]

(* [pp v] printed on a formatter in [theme], at [margin]. *)
let printed ?(theme = theme) ~margin pp v =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Format.pp_set_margin ppf margin;
  pp ppf v;
  Format.pp_print_flush ppf ();
  String.split_on_char '\n' (Buffer.contents buf)

let table ~width = Table.to_span ~width:(min width 100) steps

(* A view is a span: here a table above the default view, the tree of the
   tasks and then the summary, which stays the last line. The view is given
   the terminal's width and its height less one. *)
let test_view () =
  let t = terminal ~theme ~width:40 ~height:12 ~summary:"Build" () in
  let heights = ref [] in
  Display.set_view t.display (fun d ~width ~height ->
      heights := height :: !heights;
      above (table ~width) d ~width ~height);
  ignore (Display.task t.display "compile");
  Display.redraw t.display;
  let table = printed ~margin:40 Table.pp steps in
  let rows = screen t in
  let n = List.length table in
  check_lines "the table first" table (List.filteri (fun i _ -> i < n) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows n));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows (n + 1)));
  Alcotest.(check int) "the room" 11 (List.hd !heights)

(* A view line wider than the terminal is cut, and a view taller than the
   room loses its first lines, so that the summary at its bottom stays. *)
let test_view_fits () =
  let t = terminal ~theme ~width:20 ~height:6 ~summary:"Build" () in
  let tall = Span.text "a view line much wider than twenty\nb\nc\nd\ne\nf" in
  Display.set_view t.display (fun d ~width ~height:_ ->
      Span.vcat [ tall; Display.default_view d ~width ~height:2 ]);
  ignore (Display.task t.display "compile");
  Display.redraw t.display;
  let rows = screen t in
  check_lines "the last five lines" [ "d"; "e"; "f" ]
    (List.filteri (fun i _ -> i < 3) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows 3));
  Alcotest.(check bool)
    "the summary" true
    (contains "[-] Build" (List.nth rows 4));
  Alcotest.(check int) "within the screen less one" 5 (List.length rows);
  Display.set_view t.display (fun _ ~width:_ ~height:_ ->
      Span.text "a view line much wider than twenty\nb");
  check_lines "cut to the width"
    [ "a view line much wi\u{2026}"; "b" ]
    (screen t);
  Display.close t.display

(* On a wide terminal the view is given its width, and the tree keeps to
   100 columns. *)
let test_view_width () =
  let t = terminal ~theme ~width:160 () in
  let given = ref 0 in
  Display.set_view t.display (fun d ~width ~height ->
      given := width;
      Display.default_view d ~width ~height);
  ignore (Display.task t.display "compile");
  Display.redraw t.display;
  Alcotest.(check int) "the width" 160 !given;
  Alcotest.(check int) "the row" 100 (Width.string_width (List.hd (screen t)))

(* Widgets side by side make a view, each at the width it is given. *)
let test_view_hcat () =
  let t = terminal ~theme ~width:40 ~height:12 ~summary:"Build" () in
  let panel = Panel.v (Span.text "status") in
  let side width =
    let half = (width - 2) / 2 in
    Span.hcat
      [ Table.to_span ~width:half steps; Panel.to_span ~width:half panel ]
  in
  Display.set_view t.display (fun d ~width ~height ->
      above (side width) d ~width ~height);
  Display.redraw t.display;
  let want = printed ~margin:40 Span.pp (side 40) in
  let rows = screen t in
  check_lines "side by side" want
    (List.filteri (fun i _ -> i < List.length want) rows);
  Alcotest.(check bool)
    "on one line" true
    (contains "step" (List.nth rows 1) && contains "status" (List.nth rows 1))

(* A view replaces the one before, and its last frame stays once the
   display closes. *)
let test_view_replaced () =
  let t = terminal ~theme ~width:30 () in
  Display.set_view t.display (fun _ ~width:_ ~height:_ -> Span.text "first");
  Display.redraw t.display;
  Display.set_view t.display (fun _ ~width:_ ~height:_ -> Span.text "second");
  check_lines "the second view, at once" [ "second" ] (screen t);
  Display.close t.display;
  check_lines "its last frame stays" [ "second" ] (lines t)

(* Without a terminal a view is drawn once, when the display closes, after
   the tasks' lines. *)
let test_view_plain () =
  let (), out =
    plain ~theme ~summary:"Build" (fun d _ ->
        Display.set_view d (fun d ~width ~height ->
            above (Span.text "TOP") d ~width ~height);
        Display.succeed (Display.task d "compile"))
  in
  check_lines "the last frame, at the end"
    [ "[-] compile"; "[+] compile  0.0s"; "TOP"; "[+] Build 0.0s (1/1)"; "" ]
    (split out)

let border_glyphs =
  List.concat_map
    (fun b ->
      let c = Border.chars b in
      [
        c.top_left;
        c.top;
        c.top_right;
        c.left;
        c.right;
        c.bottom_left;
        c.bottom;
        c.bottom_right;
        c.cross;
        c.top_cross;
        c.bottom_cross;
        c.left_cross;
        c.right_cross;
      ])
    Border.[ ascii; single; rounded; double; heavy ]

let clusters = Width.clusters

(* A table's frame moves in a live display drawn in a theme with motion:
   from one frame to the next, only its border cells change, each to a glyph
   as wide, so that every line keeps its width and its cells their place. *)
let test_view_moves () =
  let run theme ~styled =
    let t = terminal ~theme ~styled ~width:40 ~height:12 () in
    Display.set_view t.display (fun _ ~width ~height:_ -> table ~width);
    Display.redraw t.display;
    let frames =
      List.init 12 (fun _ ->
          let before = List.length !(t.chunks) in
          t.advance 0.25;
          Display.redraw t.display;
          (List.length !(t.chunks) > before, screen t))
    in
    Display.close t.display;
    frames
  in
  let still = printed ~theme:Theme.matrix ~margin:40 Table.pp steps in
  let frames = run Theme.matrix ~styled:true in
  Alcotest.(check bool)
    "frames are drawn" true
    (List.exists (fun (wrote, _) -> wrote) frames);
  List.iter
    (fun (_, rows) ->
      List.iteri
        (fun i (still, row) ->
          let a = clusters still and b = clusters row in
          if List.length a <> List.length b then
            Alcotest.failf "row %d: %S became %S" i still row;
          List.iter2
            (fun a b ->
              if
                a <> b
                && not
                     (List.mem a border_glyphs
                     && Width.string_width a = Width.string_width b)
              then Alcotest.failf "row %d: %S became %S" i still row)
            a b)
        (List.combine still
           (List.filteri (fun i _ -> i < List.length still) rows)))
    frames;
  let unmoved frames =
    List.for_all (fun (wrote, rows) -> (not wrote) && rows = still) frames
  in
  Alcotest.(check bool)
    "still without colour" true
    (unmoved (run Theme.matrix ~styled:false));
  Alcotest.(check bool)
    "still once stilled" true
    (unmoved (run (Theme.still Theme.matrix) ~styled:true))

(* A table in [Border.simple] has no frame to move: in a theme with motion,
   every frame of a view is the still one. *)
let test_simple_view_stands_still () =
  let simple =
    Table.v ~border:Border.simple
      [
        Table.column (Span.text "step");
        Table.column ~align:`Right (Span.text "time");
      ]
      [
        [ Span.text "fetch"; Span.text "1.2s" ];
        [ Span.text "compile"; Span.text "3.4s" ];
      ]
  in
  List.iter
    (fun theme ->
      let t = terminal ~theme ~styled:true ~width:40 ~height:12 () in
      Display.set_view t.display (fun _ ~width ~height:_ ->
          Table.to_span ~width simple);
      Display.redraw t.display;
      let still = screen t in
      for _ = 1 to 12 do
        t.advance 0.25;
        Display.redraw t.display;
        Alcotest.(check (list string)) "still" still (screen t)
      done;
      Display.close t.display)
    Theme.[ matrix; rainbow; tokyo_night ]

(* A broken view must not prevent teardown from restoring the terminal: it
   is dropped, and the default view drawn in its place. *)
let test_view_exception_close () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  ignore (Display.task t.display "running");
  (match
     Display.set_view t.display (fun _ ~width:_ ~height:_ -> raise Exit)
   with
  | () -> Alcotest.fail "view exception was swallowed"
  | exception Exit -> ());
  Alcotest.(check bool)
    "the default view drawn" true
    (List.exists (contains "running") (screen t));
  let raised =
    try
      Display.close t.display;
      false
    with Exit -> true
  in
  Alcotest.(check bool)
    "closing does not call the broken view again" false raised;
  Alcotest.(check bool)
    "cursor restored after the view exception" true
    (Console_vte.cursor_visible t.vte)

(* {1 What a user sees} *)

(* On a narrow terminal the label is what tells two rows apart: it gives
   way last, after the bar, and the row still ends with its timer. *)
let test_label_before_bar () =
  let t = terminal ~theme ~width:40 ~height:8 () in
  let step = Display.task t.display "RUN make" in
  let layer = Display.task ~parent:step t.display "sha256:abcd" in
  Display.set_count step ~cur:5 ~total:5;
  Display.set_bytes layer ~cur:268435456L ~total:2147483648L;
  t.advance 1.5;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the parent's label shows" true
    (List.exists (contains "RUN") rows);
  Alcotest.(check bool)
    "the child's label shows" true
    (List.exists (contains "sha") rows);
  Alcotest.(check bool)
    "the timers show" true
    (List.for_all
       (fun r -> (not (contains "\xe2\x80\xa6" r)) || contains "1.5s" r)
       rows);
  Display.close t.display

(* The same in two trees side by side, as examples/columns lays them out on
   75 columns. *)
let test_label_before_bar_hcat () =
  let t = terminal ~theme ~width:75 ~height:12 () in
  let view d ~width ~height =
    let half = (min width 100 - 2) / 2 in
    match Display.tasks d with
    | [ a; b ] ->
        Span.hcat
          [
            Display.tree ~width:half ~height a;
            Display.tree ~width:half ~height b;
          ]
    | _ -> Display.default_view d ~width ~height
  in
  Display.set_view t.display view;
  let a = Display.task t.display "linux"
  and b = Display.task t.display "macos" in
  let ca = Display.task ~parent:a t.display "compile" in
  let cb = Display.task ~parent:b t.display "compile" in
  Display.set_count ca ~cur:4 ~total:12;
  Display.set_count cb ~cur:2 ~total:12;
  Display.redraw t.display;
  Alcotest.(check bool)
    "compile shows in a 36-cell tree" true
    (List.exists (contains "comp") (screen t));
  Display.close t.display

(* When the display closes, the scrollback keeps each task's label readable:
   a label that does not fit next to its count goes on below, never one
   character a line. *)
let test_last_frame_label_readable () =
  let t = terminal ~theme ~width:40 ~height:30 () in
  let step = Display.task t.display "RUN make" in
  let layer = Display.task ~parent:step t.display "sha256:abcd..." in
  Display.set_count step ~cur:5 ~total:5;
  Display.set_bytes layer ~cur:2147483648L ~total:2147483648L;
  Display.succeed layer;
  Display.succeed step;
  Display.close t.display;
  let rows = lines t in
  Alcotest.(check bool)
    "no row holds a lone character" false
    (List.exists (fun r -> String.length (String.trim r) = 1) rows);
  Alcotest.(check bool)
    "each label on one row" true
    (List.exists (contains "RUN make") rows
    && List.exists (contains "sha256") rows)

(* On a pipe, a task's output and the tasks under it are told apart by
   their paths, not by an indentation: a child's lines name its parent, and
   a line of output names its own task, two columns in. *)
let test_plain_output_not_sibling () =
  let (), out =
    plain ~theme (fun d _ ->
        let p = Display.task d "parent" in
        let c = Display.task ~parent:p d "child" in
        Display.log p "parent says";
        Display.succeed c;
        Display.succeed p)
  in
  check_lines "each line names its task"
    [
      "[-] parent";
      "[-] parent \u{203A} child";
      "  # parent: parent says";
      "[+] parent \u{203A} child  0.0s";
      "[+] parent  0.0s";
      "";
    ]
    (split out)

(* On a pipe a failure's output is printed once: the lines streamed as they
   came are not printed again under the failure. *)
let test_plain_failure_output_once () =
  let (), out =
    plain ~theme (fun d _ ->
        let job = Display.task d "job" in
        Display.log job "checksum mismatch";
        Display.fail job)
  in
  Alcotest.(check int)
    "the line once" 1
    (List.length (List.filter (contains "checksum mismatch") (split out)))

(* A task whose child failed does not read as a success. *)
let test_parent_of_failure () =
  let (), out =
    plain ~theme (fun d _ ->
        Display.with_task d "parent" (fun p ->
            Display.fail (Display.task ~parent:p d "child")))
  in
  Alcotest.(check bool)
    "no success mark on the parent" false
    (List.mem "[+] parent  0.0s" (split out));
  let t = terminal ~theme ~width:40 ~height:8 () in
  Display.with_task t.display "parent" (fun p ->
      Display.fail (Display.task ~parent:p t.display "child"));
  Display.close t.display;
  Alcotest.(check bool)
    "nor on a terminal" false
    (List.exists (fun r -> untimed r = "[+] parent") (lines t))

(* With [`Quiet] a nested failure says where it is: its parent's label goes
   with it, since its indentation alone means nothing. *)
let test_quiet_failure_context () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Display.v ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let p = Display.task d "RUN make" in
  let c = Display.task ~parent:p d "sha256" in
  Display.log c "checksum mismatch";
  Display.fail c;
  Display.succeed p;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buffer in
  Alcotest.(check bool) "the failure shows" true (contains "sha256" out);
  Alcotest.(check bool) "with its parent" true (contains "RUN make" out)

(* On a pipe, the lines of two trees that run at once interleave: a child's
   line says whose child it is, as its output lines say their task. *)
let test_plain_interleaved_children () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Display.task d "linux" and b = Display.task d "macos" in
        let fa = Display.task ~parent:a d "fetch" in
        let fb = Display.task ~parent:b d "fetch" in
        Display.succeed fa;
        Display.succeed fb;
        Display.succeed a;
        Display.succeed b)
  in
  let fetches = List.filter (contains "fetch") (split out) in
  Alcotest.(check bool)
    "each fetch line names its build" true
    (List.for_all (fun l -> contains "linux" l || contains "macos" l) fetches)

(* With [`Quiet] only what fails shows, and a transient task that fails is
   no exception: its output is the run's only trace of the failure. *)
let test_quiet_transient_failure () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Display.v ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let t = Display.task ~transient:true d "probe" in
  Display.log t "fatal: disk full";
  Display.fail t;
  Display.close d;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "the failure's output shows" true
    (contains "disk full" (Buffer.contents buffer))

let suite =
  ( "display",
    [
      Alcotest.test_case "a view exception still allows close" `Quick
        test_view_exception_close;
      Alcotest.test_case "a view side by side" `Quick test_view_hcat;
      Alcotest.test_case "a simple table stands still" `Quick
        test_simple_view_stands_still;
      Alcotest.test_case "plain lines" `Quick test_plain_lines;
      Alcotest.test_case "plain attribution" `Quick test_plain_attribution;
      Alcotest.test_case "plain unfinished tasks" `Quick test_plain_unfinished;
      Alcotest.test_case "a plain closing line says how it ended" `Quick
        test_plain_outcome_word;
      Alcotest.test_case "plain lines start a line" `Quick test_plain_column;
      Alcotest.test_case "plain sanitizes" `Quick test_plain_sanitizes;
      Alcotest.test_case "plain result" `Quick test_plain_result;
      Alcotest.test_case "empty live header remains pending" `Quick
        test_empty_header_pending;
      Alcotest.test_case "region" `Quick test_region;
      Alcotest.test_case "a view taller than the room" `Quick test_tall_header;
      Alcotest.test_case "a raw tty" `Quick test_raw_tty;
      Alcotest.test_case "after a partial line" `Quick test_after_a_partial_line;
      Alcotest.test_case "the window" `Quick test_window;
      Alcotest.test_case "alignment" `Quick test_alignment;
      Alcotest.test_case "label truncated" `Quick test_label_truncated;
      Alcotest.test_case "capped" `Quick test_capped;
      Alcotest.test_case "the marker fits" `Quick test_marker_fits;
      Alcotest.test_case "the marker counts" `Quick test_marker_counts;
      Alcotest.test_case "with_output" `Quick test_with_output;
      Alcotest.test_case "with_output without a line end" `Quick
        test_with_output_partial;
      Alcotest.test_case "with_output moving the cursor" `Quick
        test_with_output_moves;
      Alcotest.test_case "idle redraw writes nothing" `Quick test_redraw_idle;
      Alcotest.test_case "narrowing" `Quick test_narrowing;
      Alcotest.test_case "wide" `Quick test_wide;
      Alcotest.test_case "synchronized" `Quick test_synchronized;
      Alcotest.test_case "height changes" `Quick test_height_changes;
      Alcotest.test_case "nested" `Quick test_nested;
      Alcotest.test_case "nested on a pipe" `Quick test_nested_plain;
      Alcotest.test_case "keys and transient" `Quick test_keys_and_transient;
      Alcotest.test_case "tab stops in a task's output" `Quick test_tabs;
      Alcotest.test_case "a key shared by two with_task" `Quick
        test_with_task_shared_key;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "a terminal of any width" `Quick test_any_width;
      Alcotest.test_case "a closed display ignores calls" `Quick test_closed;
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "pipe lines" `Quick test_pipe_lines;
      Alcotest.test_case "pipe start lines" `Quick test_pipe_start;
      Alcotest.test_case "a success drops its output" `Quick test_success_drops;
      Alcotest.test_case "a failure prints its output" `Quick test_failure_block;
      Alcotest.test_case "a failure keeps 1 MiB" `Quick test_failure_cap;
      Alcotest.test_case "a failure on a pipe" `Quick test_failure_plain;
      Alcotest.test_case "a cancelled task prints its output" `Quick
        test_cancelled_output;
      Alcotest.test_case "the root task" `Quick test_root;
      Alcotest.test_case "within a task" `Quick test_within;
      Alcotest.test_case "two tasks" `Quick test_two_tasks;
      Alcotest.test_case "with_task" `Quick test_with_task;
      Alcotest.test_case "iter" `Quick test_iter;
      Alcotest.test_case "view" `Quick test_view;
      Alcotest.test_case "view fits" `Quick test_view_fits;
      Alcotest.test_case "view width" `Quick test_view_width;
      Alcotest.test_case "view replaced" `Quick test_view_replaced;
      Alcotest.test_case "view on a pipe" `Quick test_view_plain;
      Alcotest.test_case "view moves" `Quick test_view_moves;
      Alcotest.test_case "the label gives way after the bar" `Quick
        test_label_before_bar;
      Alcotest.test_case "the label gives way after the bar, side by side"
        `Quick test_label_before_bar_hcat;
      Alcotest.test_case "the last frame keeps labels readable" `Quick
        test_last_frame_label_readable;
      Alcotest.test_case "a pipe's output is not a sibling of the children"
        `Quick test_plain_output_not_sibling;
      Alcotest.test_case "a pipe prints a failure's output once" `Quick
        test_plain_failure_output_once;
      Alcotest.test_case "the parent of a failure is no success" `Quick
        test_parent_of_failure;
      Alcotest.test_case "a quiet failure names its parent" `Quick
        test_quiet_failure_context;
      Alcotest.test_case "a pipe names the parent of an interleaved child"
        `Quick test_plain_interleaved_children;
      Alcotest.test_case "a quiet transient failure shows" `Quick
        test_quiet_transient_failure;
    ] )
