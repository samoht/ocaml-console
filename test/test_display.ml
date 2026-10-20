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

(* {1 Without a terminal} *)

let test_plain_lines () =
  let (), out =
    plain ~theme ~header:"Building" (fun d advance ->
        let t = Display.task d "compile" in
        Display.set_count t ~cur:2 ~total:2;
        Display.log t "compiled parser.ml";
        advance 1.5;
        Display.log t "slow";
        Display.succeed t;
        Display.report d `Info "done")
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
  Display.report d `Info "next";
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
    [ "[-] left"; "[~] left  0.0s"; "" ]
    (String.split_on_char '\n' out);
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Display.task d "left");
        Display.set_result d (Ok "done"))
  in
  check_lines "succeeded with the work"
    [ "[-] left"; "[+] left  0.0s"; "[+] done"; "" ]
    (String.split_on_char '\n' out)

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
      "[x] a  0.0s";
      "  # one";
      "  # three";
      "[~] b  0.0s";
      "";
    ]
    (String.split_on_char '\n' out)

let test_plain_sanitizes () =
  let (), out =
    plain ~header:"Build\027[2J" (fun d _ ->
        let task = Display.task d "a\tb" in
        Display.log task "x\007y";
        Display.succeed task)
  in
  Alcotest.(check bool) "no escape" false (String.contains out '\027');
  Alcotest.(check bool) "log text" true (contains "x y" out)

let test_plain_result () =
  let (), out =
    plain ~theme ~header:"Work" (fun d _ ->
        ignore (Display.task d "left running");
        Display.set_result ~detail:"see the log" d (Error "it broke");
        Display.set_result d (Ok "changed my mind"))
  in
  check_lines "running tasks cancelled, verdict last"
    [
      "[-] left running";
      "[~] left running  0.0s";
      "[x] Work 0.0s (1/1)";
      "see the log";
      "[x] it broke";
      "";
    ]
    (String.split_on_char '\n' out)

let test_empty_header_pending () =
  let t = terminal ~header:"Building" () in
  Alcotest.(check bool)
    "pending before any task" true
    (List.exists (contains "\xe2\x80\xa2 Building 0.0s (0/0)") (screen t));
  Display.close t.display;
  Alcotest.(check bool)
    "succeeded once closed" true
    (List.exists (contains "\xe2\x9c\x93 Building 0.0s (0/0)") (lines t))

(* {1 On a terminal} *)

let test_region () =
  let t = terminal ~theme ~width:40 ~header:"Build" () in
  let a = Display.task t.display "fetch" in
  let b = Display.task t.display "compile" in
  Display.set_count b ~cur:1 ~total:4;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool) "header" true (contains "[-] Build" (List.nth rows 0));
  Alcotest.(check bool) "first row" true (contains "fetch" (List.nth rows 1));
  Alcotest.(check bool) "second row" true (contains "compile" (List.nth rows 2));
  Alcotest.(check bool) "its count" true (contains "1/4" (List.nth rows 2));
  Display.succeed a;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the ended task is above the region" true
    (String.starts_with ~prefix:"[+] fetch" (List.nth rows 0));
  Alcotest.(check bool)
    "header below it" true
    (contains "[-] Build" (List.nth rows 1));
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

(* With the tty in raw mode a line feed keeps the column, so a line that
   began with a bare line feed would start where the one above it ended: the
   screen of a raw tty is that of a cooked one. *)
let test_raw_tty () =
  let run raw =
    let t = terminal ~theme ~width:40 ~header:"Build" ~raw () in
    let a = Display.task t.display "fetch" in
    let b = Display.task t.display "compile" in
    Display.set_count b ~cur:1 ~total:4;
    Display.redraw t.display;
    let live = screen t in
    Display.succeed a;
    Display.report t.display `Info "note";
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
    let t = terminal ~theme ~width:40 ~before ~header:(String.make 28 'h') () in
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
      Display.report t.display `Info "note";
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

let test_capped () =
  let t = terminal ~theme ~width:30 ~height:5 ~header:"H" () in
  let tasks =
    List.init 8 (fun i -> Display.task t.display (Fmt.str "task %d" i))
  in
  Display.redraw t.display;
  let rows = List.filter (( <> ) "") (screen t) in
  Alcotest.(check int) "no taller than the screen less one" 4 (List.length rows);
  Alcotest.(check bool) "header" true (contains "[-] H" (List.nth rows 0));
  Alcotest.(check bool) "first task" true (contains "task 0" (List.nth rows 1));
  Alcotest.(check bool) "marker" true (contains "more lines" (List.nth rows 2));
  Alcotest.(check bool) "latest task" true (contains "task 7" (List.nth rows 3));
  List.iter Display.cancel tasks;
  Display.close t.display

(* Eight running tasks with a log line each fill a budget of eight lines
   exactly: the tails go, the marker takes a row and counts what is hidden. *)
let test_marker_counts () =
  let t = terminal ~theme ~width:30 ~height:10 ~header:"H" () in
  let tasks =
    List.init 8 (fun i ->
        let task = Display.task ~log_lines:1 t.display (Fmt.str "task %d" i) in
        Display.log task "line";
        task)
  in
  Display.redraw t.display;
  let rows = List.filter (( <> ) "") (screen t) in
  Alcotest.(check int) "no taller than the screen less one" 9 (List.length rows);
  let hidden = 16 - (List.length rows - 2) in
  Alcotest.(check bool)
    "the marker counts the hidden lines" true
    (List.exists (Fmt.kstr contains "%d more lines" hidden) rows);
  List.iter Display.cancel tasks;
  Display.close t.display

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
  let t = terminal ~theme ~width:40 ~height:8 ~header:"Header" () in
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
  Display.fail p;
  let late = Display.task ~parent:p t.display "late" in
  Display.succeed late;
  Display.close t.display;
  check_lines "each task its own line, the children first"
    [ "  \u{2713} one"; "  \u{25cb} two"; "\u{2717} parent"; "  \u{25cb} late" ]
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
      "  [-] one";
      "  [-] two";
      "  [+] one  0.0s";
      "  [~] two  0.0s";
      "[x] parent  0.0s";
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

(* A fiber that outlives the display, or a library that holds on to it,
   is ignored once it closed: nothing raises and nothing is written. *)
let test_closed () =
  let t = terminal ~theme ~header:"Build" () in
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
  Display.set_scene d (fun ~height:_ ppf -> Fmt.string ppf "scene");
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
  let t = terminal ~header:"Build" () in
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
            Display.report d `Info "note";
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
    [ "[-] fetch"; "  [-] index"; "" ]
    (split (Buffer.contents buffer));
  Display.close d

(* A Logs message inside a task is its output, as a line given to [log]
   is: it shows in the task's window, goes when the task succeeds, even
   once pushed out of the window, and is printed under the task's line,
   after the mark of its level, when it fails. *)
let test_logs_in_task () =
  let run ~fails =
    let t = terminal ~theme ~width:30 ~height:10 () in
    Display.with_task ~log_lines:1 t.display "job" (fun job ->
        Display.report t.display `Warning "first";
        Display.log job "out";
        Display.redraw t.display;
        Alcotest.(check bool)
          "pushed out, not above" false
          (List.exists (contains "first") (screen t));
        Display.report t.display `Info "last";
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
      (fun (level, text) -> Display.report d level text)
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
    (("[-] running" :: expected) @ [ "[~] running  0.0s"; "" ])
    (split out);
  let (), out = plain (fun d _ -> Display.report d `Debug "debug") in
  check_lines "the default theme's debug mark" [ "\u{00b7} debug"; "" ]
    (split out)

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

(* On a pipe nothing is kept: every line was printed when it came, and the
   last [log_lines] are repeated under the failure. *)
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
    "every line once, two repeated, three more"
    (n + 2 + 3)
    (List.length lines);
  let tail = List.filteri (fun i _ -> i >= n + 1) lines in
  check_lines "the last two under the failure"
    [
      "[x] job  0.0s";
      "  # line1998 " ^ String.make 600 'x';
      "  # line1999 " ^ String.make 600 'x';
      "";
    ]
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
    [ "[-] job"; "  # job: a"; "[~] job  0.0s"; "" ]
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
         Display.within root (fun () -> Display.report d `Warning "above");
         Display.log root "also above";
         Display.redraw d;
         check_lines "above the rows at once"
           [ "! above"; "also above" ]
           (List.filteri (fun i _ -> i < 2) (screen t));
         Alcotest.(check bool)
           "back in the task" true
           (match Display.current d with Some c -> c == job | None -> false);
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
                Display.report d `Info "above";
                Display.succeed (Display.task d "top"));
            Display.report d `Info "inside"))
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
                  Display.report d `Info "for a";
                  ignore (Display.task d "under a");
                  7)
            in
            Alcotest.(check int) "its value" 7 v;
            Display.report d `Info "for b");
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
      "  [-] under a";
      "  b: for b";
      "[+] b  0.0s";
      "  # a: still runs";
      "  [+] under a  0.0s";
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
            Display.with_task d "inner" (fun _ -> Display.report d `Info "deep");
            ignore (Display.task d "child");
            Display.with_task d "ended" (fun t ->
                Display.fail t;
                Display.report d `Info "after its end";
                Display.log t "ignored");
            42))
  in
  Alcotest.(check int) "its value" 42 v;
  check_lines "nested under the current task"
    [
      "[-] outer";
      "  [-] inner";
      "    inner: deep";
      "  [+] inner  0.0s";
      "  [-] child";
      "  [-] ended";
      "  [x] ended  0.0s";
      "    ended: after its end";
      "  [+] child  0.0s";
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

(* A driver's scope says which task is current, here a fiber's own. *)
let test_scope () =
  let current = ref None in
  let scope =
    Display.scope
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
  let d = Display.v ~scope ~now:(fun () -> 0.) ppf in
  let a = Display.task d "a" in
  current := Some a;
  Display.report d `Info "in a";
  current := None;
  let is task = function Some c -> c == task | None -> false in
  Display.with_task d "b" (fun b ->
      Alcotest.(check bool) "current within" true (is b !current);
      Alcotest.(check bool)
        "the display's current task" true
        (is b (Display.current d));
      Display.succeed b;
      Alcotest.(check bool)
        "none once it ended" true
        (Option.is_none (Display.current d)));
  Alcotest.(check bool) "none outside" true (Option.is_none (Display.current d));
  Display.close d;
  Format.pp_print_flush ppf ();
  check_lines "the scope's current task"
    [ "[-] a"; "  a: in a"; "[-] b"; "[+] b  0.0s"; "[~] a  0.0s"; "" ]
    (split (Buffer.contents buffer))

(* {1 Scenes} *)

let steps =
  Table.v
    [ Table.column "step"; Table.column ~align:`Right "time" ]
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

(* A scene is what its printer prints, drawn above the header in the
   display's theme, at the width of the rows; it is given the lines the
   header and the rows leave. *)
let test_scene () =
  let t = terminal ~theme ~width:40 ~height:12 ~header:"Build" () in
  let heights = ref [] in
  Display.set_scene t.display (fun ~height ppf ->
      heights := height :: !heights;
      Table.pp ppf steps);
  ignore (Display.task t.display "compile");
  Display.redraw t.display;
  let table = printed ~margin:40 Table.pp steps in
  let rows = screen t in
  let n = List.length table in
  check_lines "the table first" table (List.filteri (fun i _ -> i < n) rows);
  Alcotest.(check bool)
    "the header" true
    (contains "[-] Build" (List.nth rows n));
  Alcotest.(check bool)
    "the row" true
    (contains "compile" (List.nth rows (n + 1)));
  Alcotest.(check int) "the room left" 9 (List.hd !heights)

(* A scene line wider than the rows is cut, and a scene taller than the room
   loses its last lines; the rows keep their place. *)
let test_scene_fits () =
  let t = terminal ~theme ~width:20 ~height:6 ~header:"Build" () in
  Display.set_scene t.display (fun ~height:_ ppf ->
      Fmt.pf ppf "a scene line much wider than twenty@\nb@\nc@\nd@\ne@\nf");
  ignore (Display.task t.display "compile");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check int) "within the screen less one" 5 (List.length rows);
  Alcotest.(check string) "cut" "a scene line much w\u{2026}" (List.nth rows 0);
  check_lines "the first lines" [ "b"; "c" ]
    [ List.nth rows 1; List.nth rows 2 ];
  Alcotest.(check bool)
    "the header" true
    (contains "[-] Build" (List.nth rows 3));
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows 4))

(* On a wide terminal a scene is as wide as the rows: 100 columns. *)
let test_scene_width () =
  let t = terminal ~theme ~width:160 () in
  let margin = ref 0 in
  Display.set_scene t.display (fun ~height:_ ppf ->
      margin := Format.pp_get_margin ppf ());
  Display.redraw t.display;
  Alcotest.(check int) "the margin" 100 !margin

(* Blocks side by side make a scene, each at its share of the rows' width. *)
let test_scene_hcat () =
  let t = terminal ~theme ~width:40 ~height:12 ~header:"Build" () in
  let panel = Panel.v (Span.text "status") in
  Display.set_scene t.display (fun ~height:_ ppf ->
      hcat [ Fmt.const Table.pp steps; Fmt.const Panel.pp panel ] ppf ());
  Display.redraw t.display;
  let want =
    printed ~margin:40
      (fun ppf () ->
        hcat [ Fmt.const Table.pp steps; Fmt.const Panel.pp panel ] ppf ())
      ()
  in
  let rows = screen t in
  check_lines "side by side" want
    (List.filteri (fun i _ -> i < List.length want) rows);
  Alcotest.(check bool)
    "on one line" true
    (contains "step" (List.nth rows 1) && contains "status" (List.nth rows 1))

(* A scene replaces the one before; the display's formatter keeps its own
   margin. *)
let test_scene_replaced () =
  let t = terminal ~theme ~width:30 () in
  Display.set_scene t.display (fun ~height:_ ppf -> Fmt.string ppf "first");
  Display.redraw t.display;
  Display.set_scene t.display (fun ~height:_ ppf -> Fmt.string ppf "second");
  Display.redraw t.display;
  check_lines "the second scene" [ "second" ] (screen t);
  Display.close t.display;
  check_lines "nothing left once closed" [] (lines t)

(* Without a terminal a scene is never drawn. *)
let test_scene_plain () =
  let (), out =
    plain ~theme (fun d _ ->
        Display.set_scene d (fun ~height:_ ppf -> Table.pp ppf steps);
        Display.succeed (Display.task d "compile"))
  in
  Alcotest.(check bool) "no scene" false (contains "fetch" out)

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

let clusters s =
  let l = ref [] in
  Glyph.String.iter_graphemes
    (fun ~offset ~len -> l := String.sub s offset len :: !l)
    s;
  List.rev !l

(* A table's frame moves in a live display drawn in a theme with motion:
   from one frame to the next, only its border cells change, each to a glyph
   as wide, so that every line keeps its width and its cells their place. *)
let test_scene_moves () =
  let run theme ~styled =
    let t = terminal ~theme ~styled ~width:40 ~height:12 () in
    Display.set_scene t.display (fun ~height:_ ppf -> Table.pp ppf steps);
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
   every frame of a scene is the still one. *)
let test_simple_scene_stands_still () =
  let simple =
    Table.v ~border:Border.simple
      [ Table.column "step"; Table.column ~align:`Right "time" ]
      [
        [ Span.text "fetch"; Span.text "1.2s" ];
        [ Span.text "compile"; Span.text "3.4s" ];
      ]
  in
  List.iter
    (fun theme ->
      let t = terminal ~theme ~styled:true ~width:40 ~height:12 () in
      Display.set_scene t.display (fun ~height:_ ppf -> Table.pp ppf simple);
      Display.redraw t.display;
      let still = screen t in
      for _ = 1 to 12 do
        t.advance 0.25;
        Display.redraw t.display;
        Alcotest.(check (list string)) "still" still (screen t)
      done;
      Display.close t.display)
    Theme.[ matrix; rainbow; tokyo_night ]

(* A broken scene must not prevent teardown from restoring the terminal. *)
let test_scene_exception_close () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  ignore (Display.task t.display "running");
  (match
     Display.set_scene t.display (fun ~height:_ _ -> raise Exit);
     Display.redraw t.display
   with
  | () -> Alcotest.fail "scene exception was swallowed"
  | exception Exit -> ());
  let raised =
    try
      Display.close t.display;
      false
    with Exit -> true
  in
  Alcotest.(check bool)
    "closing does not call the broken scene again" false raised;
  Alcotest.(check bool)
    "cursor restored after the scene exception" true
    (Console_vte.cursor_visible t.vte)

let suite =
  ( "display",
    [
      Alcotest.test_case "a scene exception still allows close" `Quick
        test_scene_exception_close;
      Alcotest.test_case "a scene side by side" `Quick test_scene_hcat;
      Alcotest.test_case "a simple table stands still" `Quick
        test_simple_scene_stands_still;
      Alcotest.test_case "plain lines" `Quick test_plain_lines;
      Alcotest.test_case "plain attribution" `Quick test_plain_attribution;
      Alcotest.test_case "plain unfinished tasks" `Quick test_plain_unfinished;
      Alcotest.test_case "plain lines start a line" `Quick test_plain_column;
      Alcotest.test_case "plain sanitizes" `Quick test_plain_sanitizes;
      Alcotest.test_case "plain result" `Quick test_plain_result;
      Alcotest.test_case "empty live header remains pending" `Quick
        test_empty_header_pending;
      Alcotest.test_case "region" `Quick test_region;
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
      Alcotest.test_case "a key shared by two with_task" `Quick
        test_with_task_shared_key;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "a closed display ignores calls" `Quick test_closed;
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "pipe lines" `Quick test_pipe_lines;
      Alcotest.test_case "pipe start lines" `Quick test_pipe_start;
      Alcotest.test_case "Logs lines inside a task are its output" `Quick
        test_logs_in_task;
      Alcotest.test_case "Logs lines outside a task" `Quick test_logs_outside;
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
      Alcotest.test_case "a driver's scope" `Quick test_scope;
      Alcotest.test_case "scene" `Quick test_scene;
      Alcotest.test_case "scene fits" `Quick test_scene_fits;
      Alcotest.test_case "scene width" `Quick test_scene_width;
      Alcotest.test_case "scene replaced" `Quick test_scene_replaced;
      Alcotest.test_case "scene on a pipe" `Quick test_scene_plain;
      Alcotest.test_case "scene moves" `Quick test_scene_moves;
    ] )
