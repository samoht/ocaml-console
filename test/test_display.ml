(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let strings = Alcotest.(list string)
let check_lines msg expected actual = Alcotest.check strings msg expected actual
let theme = Theme.dos

(* {1 Without a terminal} *)

let test_plain_lines () =
  let (), out =
    plain ~theme ~header:"Building" (fun d advance ->
        let t = Display.task d "compile" in
        Display.set_count t ~cur:2 ~total:2;
        Display.log t "compiled parser.ml";
        advance 1.5;
        Display.log ~level:`Warning t "slow";
        Display.succeed t;
        Display.message d "done")
  in
  check_lines "lines in turn, the header last"
    [
      "  # compile: compiled parser.ml";
      "  ! compile: slow  +1.5s";
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
  let d = Display.v ~theme ~now:(fun () -> 0.) ppf in
  Fmt.pf ppf "partial%!";
  Display.message d "next";
  Fmt.pf ppf "again";
  Display.succeed (Display.task d "t");
  Display.close d;
  Fmt.pf ppf "after@.";
  check_lines "each display line on its own"
    [ "partial"; "next"; "again"; "[+] t  0.0s"; "after"; "" ]
    (String.split_on_char '\n' (Buffer.contents buffer))

(* A task still running when the display closes did not finish: it is
   cancelled, unless the program said the work succeeded. *)
let test_plain_unfinished () =
  let (), out = plain ~theme (fun d _ -> ignore (Display.task d "left")) in
  check_lines "cancelled" [ "[~] left  0.0s"; "" ]
    (String.split_on_char '\n' out);
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Display.task d "left");
        Display.set_result d (`Ok "done"))
  in
  check_lines "succeeded with the work"
    [ "[+] left  0.0s"; "[+] done"; "" ]
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
      "  # a: one";
      "  # b: two";
      "  # a: three";
      "[x] a  0.0s";
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
        Display.set_result ~detail:"see the log" d (`Fail "it broke");
        Display.set_result d (`Ok "changed my mind"))
  in
  check_lines "running tasks cancelled, verdict last"
    [
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
    Display.message t.display "note";
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

let test_tail () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let a = Display.task ~log_lines:2 t.display "job" in
  List.iter (Display.log a) [ "one"; "two"; "three" ];
  Display.redraw t.display;
  let rows = screen t in
  check_lines "the oldest line left the tail, above" [ "  # one"; "job" ]
    [ List.nth rows 0; String.trim (String.sub (List.nth rows 1) 2 4) ];
  Alcotest.(check bool) "tail" true (contains "# two" (List.nth rows 2));
  Alcotest.(check bool) "tail" true (contains "# three" (List.nth rows 3));
  Display.log ~keep:false a "watch only";
  Display.succeed a;
  Display.close t.display;
  check_lines "every kept line once, then the row"
    [ "  # one"; "  # two"; "  # three"; "[+] job                   0.0s" ]
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
  let all = String.concat "\n" (lines t) in
  Alcotest.(check bool)
    "parent failed" true
    (contains "\xe2\x9c\x97 parent" all);
  Alcotest.(check bool)
    "the children cancelled with it leave no line" false
    (contains "two" all || contains "late" all)

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
    [ "[+] first  0.0s"; "[+] third  0.0s"; "" ]
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
    [ "[+] count  12/12  0.0s"; "[+] bytes  1.00 MiB / 2.00 MiB  0.0s"; "" ]
    (String.split_on_char '\n' out)

(* A count near [max_int] still shows its share. *)
let test_large_counts () =
  let layout = Display.Line.(v [ label; text " "; percent ]) in
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
  check_invalid "Console.Display.Line.bar: negative width" (fun () ->
      ignore (Display.Line.bar (-1)));
  check_invalid "Console.Display.Line.v: more than one label" (fun () ->
      ignore Display.Line.(v [ label; label ]));
  Display.close d;
  Display.close d;
  check_invalid "Console.Display.task: closed display" (fun () ->
      ignore (Display.task d "late"));
  check_invalid "Console.Display.log: closed display" (fun () ->
      Display.log t "late")

let test_colours () =
  let t = terminal ~styled:true () in
  let a = Display.task t.display "job" in
  Display.fail a;
  Display.close t.display;
  let out = written t in
  (* The default theme's red is entry 160 of the 256-colour palette. *)
  Alcotest.(check bool) "red failure" true (contains "\027[1;38;5;160m" out);
  Alcotest.(check bool) "styles closed" true (contains "\027[0m" out)

let suite =
  ( "display",
    [
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
      Alcotest.test_case "tail" `Quick test_tail;
      Alcotest.test_case "alignment" `Quick test_alignment;
      Alcotest.test_case "label truncated" `Quick test_label_truncated;
      Alcotest.test_case "capped" `Quick test_capped;
      Alcotest.test_case "the marker fits" `Quick test_marker_fits;
      Alcotest.test_case "with_output" `Quick test_with_output;
      Alcotest.test_case "with_output without a line end" `Quick
        test_with_output_partial;
      Alcotest.test_case "idle redraw writes nothing" `Quick test_redraw_idle;
      Alcotest.test_case "narrowing" `Quick test_narrowing;
      Alcotest.test_case "height changes" `Quick test_height_changes;
      Alcotest.test_case "nested" `Quick test_nested;
      Alcotest.test_case "keys and transient" `Quick test_keys_and_transient;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "colours" `Quick test_colours;
    ] )
