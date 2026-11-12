(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness
open View_harness

let test_empty_header_pending () =
  let t = terminal ~label:"Building" () in
  Alcotest.(check bool)
    "pending before any task" true
    (List.exists (contains "\xe2\x80\xa2 Building 0.0s (0/0)") (screen t));
  close t;
  Alcotest.(check bool)
    "succeeded once closed" true
    (List.exists (contains "\xe2\x9c\x93 Building 0.0s (0/0)") (lines t))

let test_region () =
  let t = terminal ~theme ~width:40 ~label:"Build" () in
  let a = Task.start t.root "fetch" in
  let b = Task.start t.root "compile" in
  Task.set_items b ~count:1 ~total:4;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool) "first row" true (contains "fetch" (List.nth rows 0));
  Alcotest.(check bool) "second row" true (contains "compile" (List.nth rows 1));
  Alcotest.(check bool) "its count" true (contains "1/4" (List.nth rows 1));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows 2));
  Task.succeed a;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the ended task is above the region" true
    (String.starts_with ~prefix:"[+] fetch" (List.nth rows 0));
  Alcotest.(check bool)
    "the summary still last" true
    (contains "[-] Build" (List.nth rows 2));
  Task.succeed b;
  close t;
  check_lines "final screen"
    [
      "[+] fetch                           0.0s";
      "[+] compile \
       \u{2588}\u{2588}\u{2588}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}     \
       1/4   0.0s";
      "[+] Build 0.0s (2/2)";
    ]
    (lines t);
  Alcotest.(check bool) "cursor shown" true (contains "\027[?25h" (written t));
  Alcotest.(check bool)
    "no bare line feed" false
    (Re.execp (Re.Perl.compile_pat "(^|[^\r])\n") (written t))

(* In raw mode a line feed keeps the column. The display never uses a line
   feed to return the cursor, so a raw tty shows the same screen as a cooked
   one. *)
let test_raw_tty () =
  let run raw =
    let t = terminal ~theme ~width:40 ~label:"Build" ~raw () in
    let a = Task.start t.root "fetch" in
    let b = Task.start t.root "compile" in
    Task.set_items b ~count:1 ~total:4;
    Display.redraw t.display;
    let live = screen t in
    Task.succeed a;
    Driver.report t.root `Info "note";
    Task.set_items b ~count:2 ~total:4;
    Display.redraw t.display;
    let later = screen t in
    Task.succeed b;
    close t;
    (live, later, lines t)
  in
  let live, later, final = run false in
  let live', later', final' = run true in
  check_lines "the region" live live';
  check_lines "the region after a line above it" later later';
  check_lines "the final screen" final final'

(* The program left "Fetching " without a line end. The region starts on the
   next row. Its first line is nearly as wide as the screen, so it does not
   wrap, and a repaint erases all of it. *)
let test_after_a_partial_line () =
  let run before =
    let t = terminal ~theme ~width:40 ~before ~label:(String.make 28 'h') () in
    Display.redraw t.display;
    let a = Task.start t.root "fetch" in
    Display.redraw t.display;
    let first = screen t in
    Task.succeed a;
    close t;
    (first, lines t)
  in
  let first, final = run "" in
  let first', final' = run "Fetching " in
  let used rows = List.rev (List.drop_while (( = ) "") (List.rev rows)) in
  check_lines "the region below the host's line" ("Fetching" :: used first)
    (used first');
  check_lines "the final screen" ("Fetching" :: final) final'

(* While a task runs, its window shows its latest lines, both logged and
   reported. It cuts a line that is too wide. *)
let test_window () =
  let t = roomy () in
  let wide = "wide " ^ String.make 40 'w' in
  Task.run ~log_lines:3 t.root "job" (fun job ->
      List.iter (Task.log job) [ "one"; "two" ];
      Driver.report t.root `Info "note";
      Task.log job wide;
      Display.redraw t.display;
      let rows = screen t in
      Alcotest.(check bool) "the row" true (contains "job" (List.nth rows 0));
      check_lines "the latest three lines"
        [ "  # two"; "  note"; "  # " ^ String.sub wide 0 25 ^ "\u{2026}" ]
        (List.filteri (fun i _ -> i > 0) rows));
  close t;
  check_lines "a success folds them all away" [ "[+] job   0.0s" ] (lines t)

(* With more running tasks than rows, the latest tasks show after a line
   that counts the others. The summary stays last. No redraw scrolls the
   terminal. *)
let test_capped () =
  let t = terminal ~theme ~width:30 ~height:5 ~label:"H" () in
  let tasks =
    List.init 8 (fun i ->
        let task = Task.start t.root (Fmt.str "task %d" i) in
        Display.redraw t.display;
        task)
  in
  Display.redraw t.display;
  let rows = List.filter (( <> ) "") (screen t) in
  check_lines "no taller than the screen less one"
    [
      "  \u{2026} 6 more running";
      " |  task 6";
      " |  task 7";
      "[-] H 0.0s (0/8)";
    ]
    (List.map untimed rows);
  Alcotest.(check int) "nothing scrolled" 0 (Console_vte.scrollback_size t.vte);
  List.iter Task.cancel tasks;
  close t

(* Eight running tasks with one log line each fit in exactly eight rows: the
   windows are dropped and every task shows. With two rows fewer, only the
   latest tasks show, after a line that counts the others. *)
let test_marker_counts () =
  let run height =
    let t = terminal ~theme ~width:30 ~height ~label:"H" () in
    let tasks =
      List.init 8 (fun i ->
          let task = Task.start ~log_lines:1 t.root (Fmt.str "task %d" i) in
          Task.log task "line";
          task)
    in
    Display.redraw t.display;
    let rows = List.filter (( <> ) "") (screen t) in
    List.iter Task.cancel tasks;
    close t;
    List.map untimed rows
  in
  check_lines "the windows give way"
    (List.init 8 (Fmt.str " |  task %d") @ [ "[-] H 0.0s (0/8)" ])
    (run 10);
  check_lines "the latest tasks"
    [
      "  \u{2026} 3 more running";
      " |  task 3";
      " |  task 4";
      " |  task 5";
      " |  task 6";
      " |  task 7";
      "[-] H 0.0s (0/8)";
    ]
    (run 8)

let test_marker_fits () =
  let t = terminal ~theme ~width:12 ~height:4 () in
  let tasks = List.init 6 (fun i -> Task.start t.root (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  (* A row wider than the terminal would wrap onto a fourth row; there are
     three. *)
  Alcotest.(check int)
    "three rows" 3
    (List.length (List.filter (( <> ) "") (screen t)));
  List.iter Task.cancel tasks;
  close t

let test_with_output () =
  let t = terminal ~theme ~width:30 () in
  let a = Task.start t.root "job" in
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
  Task.succeed a;
  close t

(* The display ends output that has no line end before it draws the region
   again. *)
let test_with_output_partial () =
  let t = terminal ~theme ~width:30 () in
  let a = Task.start t.root "job" in
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "partial");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check string) "its own line" "partial" (List.nth rows 0);
  Alcotest.(check bool) "region below" true (contains "job" (List.nth rows 1));
  Task.succeed a;
  close t

(* Cursor moves in the program's output act above the region. The region is
   then drawn whole below them, on rows it erases first. The tty is raw, so a
   line feed does not return the cursor. *)
let test_with_output_moves () =
  List.iter
    (fun move ->
      let t = terminal ~theme ~raw:true ~width:30 () in
      let a = Task.start t.root "job" in
      Display.with_output t.display (fun ppf ->
          Fmt.pf ppf "ZZZZZZZZZZ%sZZZZZZZZZZ@." move);
      Display.redraw t.display;
      let row = List.find (contains "job") (screen t) in
      Alcotest.(check bool)
        (Fmt.str "%S: the row is its own: %S" move row)
        true
        (String.starts_with ~prefix:" |  job" row
        && not (String.contains row 'Z'));
      Task.succeed a;
      close t)
    [ "\027[A"; "\027[2A\r"; "\r"; "\027[K"; "\027[H" ]

let test_redraw_idle () =
  let t = terminal ~theme () in
  ignore (Task.start t.root "job");
  Display.redraw t.display;
  let before = writes t in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "nothing written" before (writes t);
  close t

(* When a terminal narrows, it reflows the old frame onto more rows. The
   redraw moves up over all of them. *)
let test_narrowing () =
  let t = terminal ~theme ~width:40 ~height:8 ~label:"Header" () in
  Display.print t.display (View.text "above");
  Task.set_items (Task.start t.root "job") ~count:1 ~total:4;
  Display.redraw t.display;
  (* When a terminal narrows, it reflows its content and the cursor stays on
     the last row. At 20 columns, the task's row of 40 cells and the summary
     of 21 take four rows. *)
  let wide = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed wide (written t);
  let held = List.filter (( <> ) "") (Console_vte.screen wide) in
  let vte = Console_vte.v ~rows:8 ~cols:20 () in
  Console_vte.feed vte (String.concat "\r\n" held);
  t.chunks := [];
  resize t ~width:20 ~height:8;
  Display.redraw t.display;
  Console_vte.feed vte (written t);
  let rows = Console_vte.screen vte in
  Alcotest.(check bool)
    "the line above is kept" true
    (List.exists (contains "above") rows);
  Alcotest.(check int)
    "the old summary is gone" 1
    (List.length (List.filter (contains "Header") rows));
  Alcotest.(check bool)
    "the old bar is gone" false
    (List.exists (contains "\u{2591}") rows);
  close t

let test_height_changes () =
  let t = terminal ~theme ~width:30 ~height:4 () in
  let tasks = List.init 5 (fun i -> Task.start t.root (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  resize t ~width:30 ~height:10;
  Display.redraw t.display;
  resize t ~width:30 ~height:3;
  Display.redraw t.display;
  Alcotest.(check bool)
    "no line wider than the terminal" true
    (List.for_all (fun r -> View.width (View.text r) <= 30) (screen t));
  List.iter Task.succeed tasks;
  close t

let test_nested () =
  let t = terminal ~width:30 ~height:10 () in
  let p = Task.start t.root "parent" in
  let c1 = Task.start p "one" in
  let _c2 = Task.start p "two" in
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "branch" true
    (contains "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80" (List.nth rows 1));
  Alcotest.(check bool)
    "last" true
    (contains "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80" (List.nth rows 2));
  Task.succeed c1;
  Display.redraw t.display;
  Alcotest.(check bool)
    "an ended child stays under its parent" true
    (contains "\u{2713} one" (List.nth (screen t) 1));
  Task.fail p;
  let late = Task.start p "late" in
  Task.succeed late;
  close t;
  check_lines
    "the parent first, each child under it, in start order, the late one too"
    [
      "\u{2717} parent";
      "  \u{251c}\u{2500}\u{2500} \u{2713} one";
      "  \u{251c}\u{2500}\u{2500} \u{25cb} two";
      "  \u{2514}\u{2500}\u{2500} \u{25cb} late";
    ]
    (List.map untimed (lines t))

let test_keys_and_transient () =
  check_plain "no line for a transient task"
    (fun d _ ->
      let a = Task.start ~key:"k" d "first" in
      let b = Task.start ~key:"k" d "second" in
      Alcotest.(check bool) "same task" true (a == b);
      Task.succeed a;
      let c = Task.start ~key:"k" d "third" in
      Alcotest.(check bool) "a new task once it ended" false (a == c);
      Task.succeed c;
      Task.succeed (Task.start ~transient:true d "gone"))
    [ "[-] first"; "[+] first  0.0s"; "[-] third"; "[+] third  0.0s"; "" ]

(* If a task with the same key is running, [Task.run] enters that task and
   ends nothing. The task ends as the call that started it says. *)
let test_with_task_shared_key () =
  check_plain "one task, ended once by the first"
    (fun d _ ->
      Task.run ~key:"k" d "first" (fun a ->
          Task.run ~key:"k" d "second" (fun b ->
              Alcotest.(check bool) "same task" true (a == b);
              Task.log b "inside");
          Task.log a "after the second";
          match Task.run ~key:"k" d "third" (fun _ -> raise Exit) with
          | () -> Alcotest.fail "the exception was swallowed"
          | exception Exit -> Task.log a "after the third"))
    [
      "[-] first";
      "  # first: inside";
      "  # first: after the second";
      "  # first: after the third";
      "[+] first  0.0s";
      "";
    ]

let test_counts () =
  check_plain "never back, total raised"
    (fun d _ ->
      let t = Task.start d "count" in
      Task.set_items t ~count:5 ~total:10;
      Task.set_items t ~count:3 ~total:8;
      Task.set_items t ~count:12 ~total:10;
      Task.succeed t;
      let b = Task.start d "bytes" in
      Task.set_bytes b ~count:1048576L ~total:2097152L;
      check_invalid "Console.Task.set_items: the task counts bytes" (fun () ->
          Task.set_items b ~count:1 ~total:1);
      Task.succeed b)
    [
      "[-] count";
      "[+] count  12/12  0.0s";
      "[-] bytes";
      "[+] bytes  1.00 MiB / 2.00 MiB  0.0s";
      "";
    ]

(* [label ~style] draws the label in [style], bold by default. *)
let test_label_style () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let task = Task.start root "job" in
  let drawn layout = View.to_string ~styled:true (Progress.row ~layout task) in
  Alcotest.(check string)
    "bold" "\027[1mjob\027[0m"
    (drawn Progress.Layout.(label ()));
  Alcotest.(check string)
    "faint" "\027[2mjob\027[0m"
    (drawn Progress.Layout.(label ~style:Style.faint ()))

(* [pp_bytes] formats a count of bytes like the count and rate of a row. *)
let test_pp_bytes () =
  List.iter
    (fun (n, s) ->
      Alcotest.(check string) s s (Fmt.str "%a" Progress.pp_bytes n))
    [
      (512L, "512 B");
      (1572864L, "1.50 MiB");
      (1023L, "1023 B");
      (1048570L, "1023.99 KiB");
      (1048575L, "1.00 MiB");
      (-1L, "0 B");
    ]

(* [advance] adds to the count and keeps the total, like indicatif's inc,
   Rich's Progress.advance and tqdm's update. *)
let test_advance () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let progress = Alcotest.testable Fmt.nop ( = ) in
  let check msg expected t =
    Alcotest.check progress msg expected (Task.progress t)
  in
  let items = Task.start root "items" in
  Task.set_items items ~count:0 ~total:3;
  Task.advance items;
  check "by one" (`Items (1, 3)) items;
  Task.advance ~by:4 items;
  check "past the total raises it" (`Items (5, 5)) items;
  let bytes = Task.start root "bytes" in
  Task.set_bytes bytes ~count:0L ~total:10L;
  Task.advance ~by:3 bytes;
  check "bytes" (`Bytes (3L, 10L)) bytes;
  let large = Task.start root "large" in
  Task.set_items large ~count:(max_int - 1) ~total:max_int;
  Task.advance ~by:2 large;
  check "the largest count" (`Items (max_int, max_int)) large;
  Alcotest.check_raises "negative"
    (Invalid_argument "Console.Task.advance: negative progress") (fun () ->
      Task.advance ~by:(-1) items);
  let spin = Task.start root "spin" in
  Alcotest.check_raises "no total"
    (Invalid_argument "Console.Task.advance: the task counts nothing")
    (fun () -> Task.advance spin);
  Task.succeed items;
  Task.advance items;
  check "an ended task keeps its count" (`Items (5, 5)) items;
  Task.advance root;
  check "the root counts nothing" `Spin root

(* [pp_duration] formats a duration like the elapsed time of a row: seconds,
   then minutes and seconds, then hours and minutes. A value that rounds up
   to the next unit is shown in that unit. *)
let test_pp_duration () =
  List.iter
    (fun (secs, s) ->
      Alcotest.(check string) s s (Fmt.str "%a" Progress.pp_duration secs))
    [
      (0., "0.0s");
      (6.44, "6.4s");
      (59.94, "59.9s");
      (59.96, "1m00s");
      (65., "1m05s");
      (3599.4, "59m59s");
      (3599.6, "1h00m");
      (3720., "1h02m");
      (359_940., "99h59m");
      (359_999., "100h00m");
      (-1., "0.0s");
      (Float.nan, "0.0s");
    ]

(* [choose f] draws each running task's row by the layout [f] gives it. *)
let test_layout_choose () =
  let layout =
    Progress.Layout.(
      choose (fun task ->
          if Task.label task = "plain" then label ()
          else list [ const (View.text "*"); label () ]))
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let a = Task.start t.root "plain" and b = Task.start t.root "starred" in
  Display.redraw t.display;
  check_lines "a layout each" [ "plain"; "*starred" ] (screen t);
  Task.succeed a;
  Task.succeed b;
  close t

(* A transient task that succeeded leaves the tree and its parent's
   children, with its line. *)
let test_transient_child_leaves () =
  let t = terminal ~theme ~width:60 () in
  let parent = Task.start t.root "resolve" in
  let child = Task.start ~transient:true parent "download" in
  Task.set_bytes child ~count:10L ~total:10L;
  t.advance 1.;
  Task.succeed child;
  Display.redraw t.display;
  Alcotest.(check bool)
    "no line in the region" false
    (List.exists (contains "download") (screen t));
  Alcotest.(check bool)
    "no line in its parent's tree" false
    (contains "download" (View.to_string (Progress.tree parent)));
  Alcotest.(check int) "no child" 0 (List.length (Task.children parent));
  Task.succeed parent;
  close t

(* A count near [max_int] still shows its percentage. *)
let test_large_counts () =
  let layout =
    Progress.Layout.(list [ label (); const (View.text " "); percent ])
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let task = Task.start t.root "big" in
  Task.set_items task ~count:(max_int / 2) ~total:max_int;
  Display.redraw t.display;
  Alcotest.(check bool)
    "half" true
    (contains " 49%" (List.nth (screen t) 0)
    || contains " 50%" (List.nth (screen t) 0));
  Task.succeed task;
  close t

(* A [const] or [custom] piece of several lines draws on one line, so each
   task takes one line of the region. *)
let test_pieces_one_line () =
  let layout =
    Progress.Layout.(
      list
        ~sep:(const (View.text " "))
        [
          label ();
          custom (fun _ -> View.text "x\ny");
          const (View.text "c1\nc2");
        ])
  in
  let t = terminal ~theme ~layout ~width:40 ~height:10 () in
  let a = Task.start t.root "a" in
  let b = Task.start t.root "b" in
  Display.redraw t.display;
  check_lines "the rows"
    [ "a x y c1 c2"; "b x y c1 c2" ]
    (List.filter (( <> ) "") (List.map String.trim (screen t)));
  Task.succeed a;
  Task.succeed b;
  close t

let test_invalid () =
  let d, display =
    show ~now:(fun () -> 0.) (Format.formatter_of_buffer (Buffer.create 1))
  in
  check_invalid "Console.Task.start: empty key" (fun () ->
      ignore (Task.start ~key:"" d "x"));
  check_invalid "Console.Task.start: negative log_lines" (fun () ->
      ignore (Task.start ~log_lines:(-1) d "x"));
  let t = Task.start d "t" in
  check_invalid "Console.Task.set_items: negative progress" (fun () ->
      Task.set_items t ~count:(-1) ~total:1);
  check_invalid "Console.Progress.Layout.bar: negative width" (fun () ->
      ignore (Progress.Layout.bar ~width:(-1) ()));
  List.iter
    (fun n ->
      check_invalid "Console.Progress.Layout.give_way: stage below 1" (fun () ->
          ignore (Progress.Layout.give_way n Progress.Layout.rate)))
    [ 0; -3 ];
  check_invalid "Console.Progress.Layout: more than one label" (fun () ->
      ignore Progress.Layout.(label () ++ label ()));
  check_invalid "Console.Progress.Layout: more than one spacer" (fun () ->
      ignore Progress.Layout.(list [ spacer; label (); spacer ]));
  Display.close display

(* [size] has no documented bound, and a terminal reports what it is told
   ($COLUMNS under [`Tty] without a terminal). A display of any width runs.
   It writes in proportion to what it draws, not to the width. *)
let test_any_width () =
  List.iter
    (fun w ->
      let buf = Buffer.create 256 in
      let ppf = Format.formatter_of_buffer buf in
      match
        let d, display =
          show ~size:(fun () -> (w, 24)) ~now:(fun () -> 0.) ppf
        in
        Task.run d "work" (fun t -> Task.set_items t ~count:1 ~total:2);
        Display.close display
      with
      | () ->
          Format.pp_print_flush ppf ();
          if Buffer.length buf > 100_000 then
            Alcotest.failf "width %d: %d bytes written" w (Buffer.length buf)
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          Fmt.epr "width %d:@." w;
          Printexc.raise_with_backtrace e bt)
    [ 1_000_000; Sys.max_string_length; max_int ]

(* A closed display ignores calls from a fiber that outlives it or a library
   that keeps it: nothing raises and nothing is written. *)
let test_closed () =
  let t = terminal ~theme ~label:"Build" () in
  let d = t.display in
  let running = Task.start t.root "running" in
  Display.close d;
  let before = written t in
  Display.close d;
  let late = Task.start t.root "late" in
  List.iter
    (fun task ->
      Task.set_label task "renamed";
      Task.set_items task ~count:1 ~total:2;
      Task.log task "late line";
      Task.succeed task;
      Task.fail task;
      Task.cancel task)
    [ running; late ];
  Display.redraw d;
  Display.set_view d (View.text "view");
  Display.print d (View.text "printed");
  Task.succeed ~message:"late" t.root;
  Alcotest.(check string) "nothing written" before (written t);
  Alcotest.(check int)
    "with_output still calls its function" 42
    (Display.with_output d (fun _ -> 42))

let test_colours () =
  let t = terminal ~styled:true () in
  let a = Task.start t.root "job" in
  Task.fail a;
  close t;
  let out = written t in
  (* The red of the default theme is the terminal's own red. *)
  Alcotest.(check bool) "red failure" true (contains "\027[1;31m" out);
  Alcotest.(check bool) "styles closed" true (contains "\027[0m" out)

(* Under a running task's row, printed lines and Logs messages below the
   warning level are muted (faint in the default theme). A warning or an
   error keeps the colour of its mark, and its text is not muted. *)
let test_log_muted () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  Task.run ~log_lines:4 t.root "job" (fun job ->
      Task.log job "split pkg/";
      Task.log ~level:`Info job "push main";
      Task.log ~level:`Warning job "disk low";
      Task.log ~level:`Error job "push failed";
      Display.redraw t.display;
      let out = written t in
      Alcotest.(check bool)
        "the printed line, faint" true
        (contains "\027[2msplit pkg/\027[0m" out);
      Alcotest.(check bool)
        "the Logs message, faint" true
        (contains "\027[2m  push main\027[0m" out);
      Alcotest.(check bool)
        "the warning, its mark yellow and its text plain" true
        (contains "\027[1;33m!\027[0m disk low\r" out);
      Alcotest.(check bool)
        "the error, its mark red and its text plain" true
        (contains "\027[1;31m\u{2717}\027[0m push failed" out));
  close t

(* The output of a failed task goes to the scrollback for reading, so it is
   not muted. *)
let test_failure_not_muted () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  let job = Task.start t.root "job" in
  Task.log job "Error: unbound value x";
  Display.redraw t.display;
  t.chunks := [];
  Task.fail job;
  let out = written t in
  Alcotest.(check bool)
    "the line, plain" true
    (contains "\027[0m Error: unbound value x\r\n" out);
  Alcotest.(check bool) "not faint" false (contains "\027[2mError" out)

(* In a theme with a muted colour, the lines under a running task's row use
   that colour, not faint. *)
let test_log_muted_colour () =
  let t = terminal ~theme:Theme.nord ~styled:true ~width:40 ~height:10 () in
  Task.run t.root "job" (fun job ->
      Task.log job "split pkg/";
      Display.redraw t.display;
      let out = written t in
      Alcotest.(check bool)
        "nord's muted colour" true
        (contains "\027[38;2;150;158;173msplit pkg/\027[0m" out);
      Alcotest.(check bool) "not faint" false (contains "\027[2m" out));
  close t

(* On a terminal wider than 100 columns, the rows and the lines of ended
   tasks use at most 100 columns, so a count and a timer stay near their
   label. *)
let test_wide () =
  let t = terminal ~width:160 ~height:8 () in
  let task = Task.start t.root ("compile " ^ String.make 62 'x') in
  Task.set_items task ~count:1 ~total:3;
  Task.log task "a log line";
  Display.redraw t.display;
  let widths () = List.map Console_width.string_width (lines t) in
  Alcotest.(check int) "the live row" 100 (List.fold_left max 0 (widths ()));
  Task.succeed task;
  close t;
  Alcotest.(check int)
    "the line it ends on" 100
    (List.fold_left max 0 (widths ()))

(* On a terminal, each write of the display is one synchronized update, so a
   terminal that supports the mode shows a whole frame. A pipe gets no such
   escape. *)
let test_synchronized () =
  let begin_ = "\027[?2026h" and end_ = "\027[?2026l" in
  let t = terminal ~label:"Build" () in
  let task = Task.start t.root "a" in
  t.advance 0.2;
  Display.redraw t.display;
  Task.log task "x";
  Task.succeed task;
  close t;
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
  let (), out = plain (fun d _ -> Task.succeed (Task.start d "a")) in
  Alcotest.(check bool) "none on a pipe" false (contains "?2026" out)

(* With [delay], the region shows only after the display has run that long.
   Printed output goes out at once. A run that ends sooner leaves only its
   last frame. *)
let test_delay () =
  let t = terminal ~theme ~label:"Build" ~delay:0.5 () in
  let job = Task.start t.root "job" in
  Display.print t.display (View.text "printed");
  Display.redraw t.display;
  check_lines "nothing yet but the print" [ "printed" ] (screen t);
  t.advance 0.6;
  Display.redraw t.display;
  Alcotest.(check bool)
    "the rows once due" true
    (List.exists (contains "job") (screen t));
  Task.succeed job;
  close t;
  let t = terminal ~theme ~label:"Build" ~delay:0.5 () in
  Task.succeed (Task.start t.root "quick");
  Display.redraw t.display;
  close t;
  Alcotest.(check bool)
    "no region for a quick work" false
    (contains "\027[?25l" (written t));
  Alcotest.(check bool)
    "its last frame" true
    (List.exists (contains "Build") (lines t))

(* A line printed by the program wraps like the terminal wraps it, mid-word.
   A line after a mark wraps between words and continues under its text. *)
let test_root_line_wraps () =
  let t = terminal ~theme ~width:10 ~height:6 () in
  Task.log t.root "Get:1 [72.7 kB]";
  Driver.report t.root `Warning "aaaa bbbbbbb";
  close t;
  check_lines "as a terminal, then under the mark"
    [ "Get:1 [72."; "7 kB]"; "! aaaa"; "  bbbbbbb" ]
    (lines t)

(* On a terminal, a task that succeeds drops all its output, including the
   lines that left its window. *)
let test_success_drops () =
  let t = roomy () in
  let job = Task.start ~log_lines:2 t.root "job" in
  List.iter (Task.log job) [ "one"; "two"; "three" ];
  Display.redraw t.display;
  Alcotest.(check bool)
    "not above while it runs" false
    (List.exists (contains "one") (screen t));
  Task.succeed job;
  close t;
  check_lines "only its line" [ "[+] job   0.0s" ] (lines t)

(* A task that fails prints all its output under its line, in one block,
   without its label and not cut. A line that is too wide continues on the
   rows below. [Task.run] fails its task when its function raises. *)
let test_failure_block () =
  let t = roomy () in
  let wide = String.make 45 'w' in
  (match
     Task.run ~log_lines:2 t.root "job" (fun job ->
         List.iter (Task.log job) [ "l1"; "l2"; "l3"; wide ];
         failwith "boom")
   with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  close t;
  let rows = lines t in
  Alcotest.(check string)
    "the failure first" "[x] job: Failure(\"boom\")"
    (untimed (List.hd rows));
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
  Alcotest.(check bool)
    "no label under the line" false
    (List.exists (contains "job:") (List.tl rows))

(* The block holds the latest lines, at most 1 MiB. One line says how many
   lines were dropped. *)
let test_failure_cap () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let job = Task.start t.root "job" in
  let n = 1200 in
  let line i = Fmt.str "line%04d%s" i (String.make 992 'x') in
  for i = 0 to n - 1 do
    Task.log job (line i)
  done;
  t.chunks := [];
  Task.fail job;
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
  close t

(* A cancelled task (Ctrl-C cancels all of them) prints all its output under
   its line, as a failure does. This includes the lines that left its
   window. *)
let test_cancelled_output () =
  let t = roomy () in
  let job = Task.start ~log_lines:2 t.root "job" in
  let other = Task.start ~log_lines:1 t.root "other" in
  List.iter (Task.log job) [ "a"; "b"; "c" ];
  Task.log other "z";
  Task.cancel job;
  Display.close t.display;
  check_lines "all of it under each line"
    [ "[~] job"; "  # a"; "  # b"; "  # c"; "[~] other"; "  # z" ]
    (List.map untimed (lines t));
  check_plain "nothing repeated on a pipe"
    (fun d _ ->
      let job = Task.start ~log_lines:2 d "job" in
      Task.log job "a";
      Task.cancel job)
    [ "[-] job"; "  # job: a"; "[~] job  0.0s  cancelled"; "" ]

(* Output logged to the root goes above the rows at once and stays there:
   while a task runs, when that task fails, and after the root ends. The
   root counts nothing, and only its first end counts. *)
let test_root () =
  let t = roomy () in
  let d = t.root in
  let root = d in
  (match
     Task.run ~log_lines:2 d "job" (fun job ->
         Task.log job "mine";
         Task.within root (fun () -> Driver.report d `Warning "above");
         Task.log root "also above";
         Display.redraw t.display;
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
  Task.set_items root ~count:1 ~total:2;
  Task.set_bytes root ~count:1L ~total:2L;
  Alcotest.(check bool)
    "the root counts nothing" true
    (Task.progress root = `Spin);
  Task.succeed root;
  Task.fail root;
  Task.cancel root;
  Alcotest.(check bool)
    "the first ending counts" true
    (Task.state root = `Succeeded);
  Task.log root "still there";
  Display.close t.display;
  check_lines "they stay, the task's own under its failure"
    [
      "! above";
      "also above";
      "[x] job: Failure(\"boom\")";
      "  # mine";
      "still there";
    ]
    (List.map untimed (lines t));
  check_plain "on a pipe, unlabelled"
    (fun d _ ->
      Task.run d "job" (fun _ ->
          Task.within d (fun () ->
              Driver.report d `Info "above";
              Task.succeed (Task.start d "top"));
          Driver.report d `Info "inside"))
    [
      "[-] job";
      "above";
      "[-] top";
      "[+] top  0.0s";
      "  job: inside";
      "[+] job  0.0s";
      "";
    ]

(* [within task f] makes the running [task] current for [f] and ends
   nothing. What [f] prints and the tasks it starts belong to [task], also
   when [f] raises. *)
let test_within () =
  check_plain "the lines of the task given"
    (fun d _ ->
      let a = Task.start d "a" in
      Task.run d "b" (fun _ ->
          let v =
            Task.within a (fun () ->
                Driver.report d `Info "for a";
                ignore (Task.start a "under a");
                7)
          in
          Alcotest.(check int) "its value" 7 v;
          Driver.report d `Info "for b");
      (match Task.within a (fun () -> failwith "inside") with
      | () -> Alcotest.fail "no exception"
      | exception Failure _ -> ());
      Task.log a "still runs";
      Task.succeed a)
    [
      "[-] a";
      "[-] b";
      "  a: for a";
      "[-] a \u{25B8} under a";
      "  b: for b";
      "[+] b  0.0s";
      "  # a: still runs";
      "[+] a \u{25B8} under a  0.0s";
      "[+] a  0.0s";
      "";
    ]

(* When two tasks run at once on a terminal, each block is printed under its
   own line and never mixed with the other. *)
let test_two_tasks () =
  let t = roomy () in
  let a = Task.start t.root "a" and b = Task.start t.root "b" in
  List.iter
    (fun (task, s) -> Task.log task s)
    [ (a, "a1"); (b, "b1"); (a, "a2"); (b, "b2") ];
  Task.fail a;
  Task.fail b;
  close t;
  check_lines "one block each"
    [ "[x] a"; "  # a1"; "  # a2"; "[x] b"; "  # b1"; "  # b2" ]
    (List.map untimed (lines t))

(* [Task.run] makes the task current for its function, so tasks and messages
   inside it go under it. The task succeeds when the function returns,
   unless the function ended it first. *)
let test_with_task () =
  let v, out =
    plain ~theme (fun d _ ->
        Task.run d "outer" (fun outer ->
            Task.run outer "inner" (fun _ -> Driver.report d `Info "deep");
            ignore (Task.start outer "child");
            Task.run outer "ended" (fun t ->
                Task.fail t;
                Driver.report d `Info "after its end";
                Task.log t "ignored");
            42))
  in
  Alcotest.(check int) "its value" 42 v;
  check_lines "nested under the task given"
    [
      "[-] outer";
      "[-] outer \u{25B8} inner";
      "  outer \u{25B8} inner: deep";
      "[+] outer \u{25B8} inner  0.0s";
      "[-] outer \u{25B8} child";
      "[-] outer \u{25B8} ended";
      "[x] outer \u{25B8} ended  0.0s  failed";
      "  outer \u{25B8} ended: after its end";
      "[+] outer \u{25B8} child  0.0s";
      "[+] outer  0.0s";
      "";
    ]
    (split out);
  check_plain "a task goes under the task given, output to the current one"
    (fun d _ ->
      Task.run d "outer" (fun _ ->
          let top = Task.start d "top" in
          Driver.report d `Info "to outer";
          Task.succeed top))
    [
      "[-] outer";
      "[-] top";
      "  outer: to outer";
      "[+] top  0.0s";
      "[+] outer  0.0s";
      "";
    ]

(* When its function raises, [Task.run] fails the task. The task gives the
   reason in its line and in its JSON end event. *)
let test_with_task_reason () =
  let run progress =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    let d, display = show ~progress ~now:(fun () -> 0.) ppf in
    (try Task.run d "fetch" (fun _ -> failwith "disk full")
     with Failure _ -> ());
    Display.close display;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check bool)
    "in the line" true
    (contains "[x] fetch: Failure(\"disk full\")" (run `Plain));
  Alcotest.(check bool)
    "in the end event" true
    (contains "\"status\":\"failed\",\"elapsed\":0,\"message\":\"fetch: Failure"
       (run `Json))

(* [children t] are the tasks under [t]; for the root, the top tasks. An
   ended task stays among them while its top task runs. *)
let test_children () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let d = t.root in
  let a = Task.start d "a" and b = Task.start d "b" in
  let a1 = Task.start a "a1" in
  let a2 = Task.start a "a2" in
  Task.succeed a1;
  Alcotest.(check int) "the top tasks" 2 (List.length (Task.children d));
  Alcotest.(check bool)
    "the tasks under a" true
    (match Task.children a with [ x; y ] -> x == a1 && y == a2 | _ -> false);
  Alcotest.(check int) "none under b" 0 (List.length (Task.children b));
  close t

(* [output] is the lines a task keeps, oldest first. [dropped] counts the
   oldest lines dropped past 1 MiB. A task that succeeded keeps no lines, and
   the root has none. *)
let test_output () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let top = Task.start t.root "top" in
  let job = Task.start top "job" in
  Task.log job "one\ntwo";
  check_lines "its lines" [ "one"; "two" ] (Task.output job);
  Alcotest.(check int) "none dropped" 0 (Task.dropped job);
  let big = String.make 1023 'x' in
  for _ = 1 to 1100 do
    Task.log job big
  done;
  let kept = Task.output job in
  Alcotest.(check int)
    "the oldest dropped" 1102
    (List.length kept + Task.dropped job);
  Alcotest.(check bool) "the latest kept" true (List.hd kept = big);
  Task.fail job;
  Alcotest.(check int)
    "a failure keeps its lines" (List.length kept)
    (List.length (Task.output job));
  let ok = Task.start top "ok" in
  Task.log ok "done";
  Task.succeed ok;
  check_lines "a success keeps none" [] (Task.output ok);
  Task.log t.root "above";
  check_lines "the root has none" [] (Task.output t.root);
  Task.succeed top;
  close t

(* [rate] is the count per second shown in the rate column. It is [None]
   before the task counts. *)
let test_rate () =
  let t = terminal ~theme ~width:100 () in
  let job = Task.start t.root "job" in
  Alcotest.(check (option (float 1e-9))) "none yet" None (Task.rate job);
  t.advance 2.;
  Task.set_bytes job ~count:2048L ~total:4096L;
  Alcotest.(check (option (float 1e-9)))
    "bytes per second" (Some 1024.) (Task.rate job);
  Display.redraw t.display;
  Alcotest.(check bool)
    "as the row shows it" true
    (List.exists (contains "1.00 KiB/s") (screen t));
  Task.succeed job;
  close t

(* A task that counts the items of a list. *)
let test_iter () =
  let seen = ref [] in
  let (), out =
    plain ~theme (fun d _ ->
        let items = [ 1; 2; 3 ] in
        let total = List.length items in
        Task.run d "items" (fun t ->
            Task.set_items t ~count:0 ~total;
            List.iteri
              (fun i x ->
                seen := x :: !seen;
                Task.set_items t ~count:(i + 1) ~total)
              items))
  in
  Alcotest.(check (list int)) "in order" [ 1; 2; 3 ] (List.rev !seen);
  check_lines "counted" [ "[-] items"; "[+] items  3/3  0.0s"; "" ] (split out)

(* A task's mark shows how the program ended it. A failed child does not
   fail its parent. A parent that the program fails is marked failed. *)
let test_parent_of_failure () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.run d "parent" (fun p -> Task.fail (Task.start p "child"));
        Task.run d "step" (fun p ->
            Task.fail (Task.start p "layer");
            Task.fail p))
  in
  check_lines "each mark as the program said"
    [
      "[-] parent";
      "[-] parent \u{25B8} child";
      "[x] parent \u{25B8} child  0.0s  failed";
      "[+] parent  0.0s";
      "[-] step";
      "[-] step \u{25B8} layer";
      "[x] step \u{25B8} layer  0.0s  failed";
      "[x] step  0.0s  failed";
      "";
    ]
    (split out);
  let t = terminal ~theme ~width:40 ~height:8 () in
  Task.run t.root "parent" (fun p -> Task.fail (Task.start p "child"));
  close t;
  Alcotest.(check bool)
    "on a terminal too" true
    (List.exists (fun r -> untimed r = "[+] parent") (lines t))

(* With [`Quiet], a nested failure shows with its parent's label, because
   indentation alone would not show where it is. *)
let test_quiet_failure_context () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let p = Task.start d "RUN make" in
  let c = Task.start p "sha256" in
  Task.log c "checksum mismatch";
  Task.fail c;
  Task.succeed p;
  Display.close display;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buffer in
  Alcotest.(check bool) "the failure shows" true (contains "sha256" out);
  Alcotest.(check bool) "with its parent" true (contains "RUN make" out)

(* With [`Quiet], only failures show, including a failed transient task.
   Its output is the only trace of the failure. *)
let test_quiet_transient_failure () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let t = Task.start ~transient:true d "probe" in
  Task.log t "fatal: disk full";
  Task.fail t;
  Display.close display;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "the failure's output shows" true
    (contains "disk full" (Buffer.contents buffer))

(* While the display is open, the summary shows the root as running, even
   when every task so far has ended. *)
let test_summary_between_tasks () =
  let t = terminal ~theme ~width:40 ~label:"Build" () in
  let a = Task.start t.root "fetch" in
  Task.succeed a;
  Display.redraw t.display;
  Alcotest.(check bool)
    "still running" true
    (List.exists (contains "[-] Build") (screen t));
  Alcotest.(check bool)
    "not a success" false
    (List.exists (contains "[+] Build") (screen t));
  close t

(* The 4 KiB cut does not split a flag, which is one grapheme cluster. *)
let test_cap_keeps_a_flag () =
  let flag = "\u{1F1EB}\u{1F1F7}" in
  let label = String.make 4089 'a' ^ flag ^ "b" in
  let (), out =
    plain (fun d _ ->
        let t = Task.start d label in
        Task.succeed t)
  in
  let has affix = contains affix out in
  Alcotest.(check bool)
    "the flag whole or not at all" true
    (has flag || not (has "\u{1F1EB}"))

let test_tty_transient_failure () =
  let t = terminal ~theme ~width:40 ~label:"S" () in
  (try
     Task.run ~transient:true t.root "probe" (fun r ->
         Task.log r "disk full";
         failwith "boom")
   with Failure _ -> ());
  close t;
  let all = lines t in
  Alcotest.(check bool)
    ("the failure shows in\n" ^ String.concat "\n" all)
    true
    (List.exists (contains "[x] probe") all
    && List.exists (contains "disk full") all)

(* [rate_of s] is the number of bytes per second in a rate text such as
   "724.00 KiB/s". *)
let rate_of s =
  match String.split_on_char ' ' (String.trim s) with
  | [ n; unit ] ->
      let k =
        match unit with
        | "B/s" -> 1.
        | "KiB/s" -> 1024.
        | "MiB/s" -> 1048576.
        | u -> Alcotest.failf "unit %S" u
      in
      float_of_string n *. k
  | _ -> Alcotest.failf "rate %S" s

(* The rate weighs each past sample by the time since it was measured. So a
   task that reports every hundredth of a second and one that reports every
   second show the same rate for the same bytes. *)
let test_rate_time_weighted () =
  let now = ref 0. in
  let root = Task.root ~now:(fun () -> !now) () in
  let often = Task.start root "often" and seldom = Task.start root "seldom" in
  (* 1 MiB/s for 3 s, then 256 KiB/s. *)
  let bytes t =
    if t <= 3. then t *. 1048576. else (3. *. 1048576.) +. ((t -. 3.) *. 262144.)
  in
  let total = 100_000_000L in
  for i = 1 to 400 do
    now := float i /. 100.;
    Task.set_bytes often ~count:(Int64.of_float (bytes !now)) ~total;
    if i mod 100 = 0 then
      Task.set_bytes seldom ~count:(Int64.of_float (bytes !now)) ~total
  done;
  let rate task =
    rate_of (View.to_string (Progress.row ~layout:Progress.Layout.rate task))
  in
  let a = rate often and b = rate seldom in
  if Float.abs (a -. b) > 0.1 *. Float.max a b then
    Alcotest.failf "%.0f B/s reported often, %.0f B/s seldom" a b;
  Alcotest.(check bool)
    "between the old rate and the new" true
    (a > 262144. && a < 1048576.)

(* The time left is the remaining count divided by the task's rate. *)
let test_eta () =
  let now = ref 0. in
  let root = Task.root ~now:(fun () -> !now) () in
  let task = Task.start root "work" in
  Task.set_items task ~count:0 ~total:4;
  now := 1.;
  Task.set_items task ~count:1 ~total:4;
  Alcotest.(check string)
    "three items at one a second" "ETA 3.0s"
    (String.trim
       (View.to_string (Progress.row ~layout:Progress.Layout.eta task)));
  let fresh = Task.start root "fresh" in
  Alcotest.(check string)
    "none without a rate" ""
    (String.trim
       (View.to_string (Progress.row ~layout:Progress.Layout.eta fresh)))

(* [on_event] sees each event of the root's tasks in order, with the root's
   end last. *)
let test_on_event () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let seen = ref [] in
  Task.on_event root (fun e -> seen := e :: !seen);
  Task.run root "a" (fun a ->
      Task.set_items a ~count:1 ~total:2;
      Task.log a "line");
  Task.succeed ~message:"done" root;
  let kinds =
    List.rev_map
      (fun e ->
        match (Task.Event.kind e, Task.Event.task e) with
        | `Start, Some (_, [ "a" ]) -> "start a"
        | `Progress, _ -> "progress"
        | `Output, Some _ -> "output"
        | `End, Some _ -> "end a"
        | `End, None -> (
            match Task.Event.message e with
            | Some m -> "end of the work: " ^ m
            | None -> "end of the work")
        | _ -> "other")
      !seen
  in
  check_lines "the events in order"
    [ "start a"; "progress"; "output"; "end a"; "end of the work: done" ]
    kinds

(* A top task leaves in the scrollback what [commit] draws for it: here one
   line per task, or nothing. *)
let test_commit () =
  let run commit =
    let t = terminal ~theme ~width:30 ~height:8 ~commit () in
    let a = Task.start t.root "a" in
    Task.succeed (Task.start a "a1");
    Task.succeed a;
    Task.succeed (Task.start t.root "b");
    close t;
    lines t
  in
  check_lines "one line a top task" [ "done a"; "done b" ]
    (run (fun t -> Some (View.text ("done " ^ Task.label t))));
  check_lines "nothing" [] (run (fun _ -> None))

(* [commit] sees a top task as it ended, including the output of a success.
   The task drops that output once committed. *)
let test_commit_output () =
  let seen = ref [] in
  let commit job =
    seen := Task.output job;
    None
  in
  let t = terminal ~theme ~width:30 ~height:8 ~commit () in
  let job = Task.start t.root "job" in
  Task.log job "one\ntwo";
  Task.succeed job;
  check_lines "its output" [ "one"; "two" ] !seen;
  check_lines "then none" [] (Task.output job);
  close t

(* [print] writes at once. On a terminal it writes above the region and
   draws the region again below. On a pipe it writes in order. *)
let test_print () =
  let t = terminal ~theme ~width:30 ~height:8 () in
  ignore (Task.start t.root "job");
  Display.redraw t.display;
  let before = writes t in
  Display.print t.display (View.text "printed");
  Alcotest.(check int) "one write" (before + 1) (writes t);
  check_lines "above the region, at once" [ "printed"; " |  job" ]
    (List.map untimed (screen t));
  close t;
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  let _, display = show ~progress:`Plain ~now:(fun () -> 0.) ppf in
  Display.print display (View.text "printed");
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "on a pipe, at once" "printed\n" (Buffer.contents buffer);
  Display.close display

(* [print_lines] puts each line of its views on a line of its own and cuts a
   line wider than the terminal, so the region below stays in place. *)
let test_print_lines_fit () =
  let t = terminal ~theme ~width:10 ~height:8 () in
  ignore (Task.start t.root "job");
  Display.redraw t.display;
  Console__.Display_core.print_lines t.display
    [ View.text "a\nb"; View.text (String.make 30 'x') ];
  check_lines "each line apart, the long one cut"
    [ "a"; "b"; "xxxxxxxxx\u{2026}"; " |  job" ]
    (List.map untimed (screen t));
  close t

(* A display that caches its rows and trees between frames shows the same
   screen as one that caches nothing. This holds as tasks start, count, log,
   are renamed and end, and as time moves the spinners and timers. A layout
   with a [custom] piece caches nothing, and an empty piece adds nothing to a
   row, so that layout gives the display that caches nothing. *)
let test_kept_rows () =
  let time = ref 0. in
  let now () = !time in
  let root = Task.root ~label:"build" ~now () in
  let screen layout =
    let vte = Console_vte.v ~rows:16 ~cols:60 () in
    let ppf = feeding vte in
    Fmt.set_utf_8 ppf true;
    ( vte,
      Display.v ~size:(fun () -> (60, 16)) ~now ppf (Progress.view ~layout root)
    )
  in
  let kept_vte, kept = screen Progress.Layout.default in
  let fresh_vte, fresh =
    screen Progress.Layout.(default ++ custom (fun _ -> View.empty))
  in
  let check step =
    Display.redraw kept;
    Display.redraw fresh;
    Alcotest.(check (list string))
      step
      (Console_vte.screen fresh_vte)
      (Console_vte.screen kept_vte)
  in
  let a = Task.start root "fetch" in
  let b = Task.start root "compile" in
  let c = Task.start b "src/a.c" in
  check "started";
  for i = 1 to 12 do
    if i mod 2 = 0 then time := !time +. 0.037;
    Task.set_items a ~count:i ~total:12;
    if i mod 3 = 0 then Task.log c (Fmt.str "line %d" i);
    Fmt.kstr check "step %d" i
  done;
  Task.set_label c "src/b.c";
  check "renamed";
  Task.succeed a;
  check "a top task ended";
  Task.fail c;
  check "a nested task failed";
  time := !time +. 0.25;
  check "the time moves";
  Task.succeed b;
  check "every task ended";
  time := !time +. 1.;
  check "the time moves on";
  Display.close kept;
  Display.close fresh

(* A display made with [Display.v] is told of no change. So each redraw lays
   out again a view that reads the tasks or the program's state, even when
   the clock does not move. A view that reads neither is laid out once. *)
let test_redraw_reads_state () =
  let now () = 0. in
  let root = Task.root ~label:"build" ~now () in
  let vte = Console_vte.v ~rows:8 ~cols:40 () in
  let ppf = feeding vte in
  let jobs = ref [] in
  let status = View.fit (fun _ -> View.text (String.concat "," !jobs)) in
  let still, calls = laid () in
  let d =
    Display.v
      ~size:(fun () -> (40, 8))
      ~now ppf
      (View.vcat [ still; status; Progress.view root ])
  in
  let shows step affix =
    Display.redraw d;
    Alcotest.(check bool)
      step true
      (List.exists (contains affix) (Console_vte.screen vte))
  in
  let a = Task.start root "fetch" in
  shows "a task started" "fetch";
  Task.set_items a ~count:3 ~total:4;
  shows "its count" "3/4";
  jobs := [ "linux"; "macos" ];
  shows "the program's state" "linux,macos";
  Alcotest.(check int) "the still view laid out once" 1 !calls;
  Display.close d

(* The display of a root's tasks is told of each change to them. A redraw
   with no change does not lay out a view of the tasks again. A view that
   reads the program's state is laid out at each redraw. *)
let test_redraw_told () =
  let t = terminal () in
  let rows, calls = laid ~tasks:true () in
  Display.set_view t.display (View.vcat [ rows; Progress.view t.root ]);
  let a = Task.start t.root "a" in
  Display.redraw t.display;
  let laid_out = !calls in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "not laid out again" laid_out !calls;
  Task.set_items a ~count:1 ~total:2;
  Display.redraw t.display;
  Alcotest.(check int) "laid out once changed" (laid_out + 1) !calls;
  Alcotest.(check bool)
    "the count shows" true
    (List.exists (contains "1/2") (screen t));
  let state, calls = laid ~live:true () in
  Display.set_view t.display (View.vcat [ state; Progress.view t.root ]);
  let laid_out = !calls in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "the state laid out at each" (laid_out + 2) !calls;
  close t

let suite =
  ( "display",
    [
      Alcotest.test_case "kept rows are the rows laid out afresh" `Quick
        test_kept_rows;
      Alcotest.test_case "a redraw draws what the view reads" `Quick
        test_redraw_reads_state;
      Alcotest.test_case "a display told of changes draws on change" `Quick
        test_redraw_told;
      Alcotest.test_case "the rate weighs time, not reports" `Quick
        test_rate_time_weighted;
      Alcotest.test_case "the time left" `Quick test_eta;
      Alcotest.test_case "a program sees the events" `Quick test_on_event;
      Alcotest.test_case "commit" `Quick test_commit;
      Alcotest.test_case "a printed line wraps as a terminal" `Quick
        test_root_line_wraps;
      Alcotest.test_case "commit sees the output" `Quick test_commit_output;
      Alcotest.test_case "print writes at once" `Quick test_print;
      Alcotest.test_case "printed lines fit the terminal" `Quick
        test_print_lines_fit;
      Alcotest.test_case "a delay before the rows" `Quick test_delay;
      Alcotest.test_case "empty live header remains pending" `Quick
        test_empty_header_pending;
      Alcotest.test_case "region" `Quick test_region;
      Alcotest.test_case "a raw tty" `Quick test_raw_tty;
      Alcotest.test_case "after a partial line" `Quick test_after_a_partial_line;
      Alcotest.test_case "the window" `Quick test_window;
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
      Alcotest.test_case "keys and transient" `Quick test_keys_and_transient;
      Alcotest.test_case "a key shared by two with_task" `Quick
        test_with_task_shared_key;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "pieces of several lines draw on one" `Quick
        test_pieces_one_line;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "a terminal of any width" `Quick test_any_width;
      Alcotest.test_case "a closed display ignores calls" `Quick test_closed;
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "a success drops its output" `Quick test_success_drops;
      Alcotest.test_case "a failure prints its output" `Quick test_failure_block;
      Alcotest.test_case "a failure keeps 1 MiB" `Quick test_failure_cap;
      Alcotest.test_case "a cancelled task prints its output" `Quick
        test_cancelled_output;
      Alcotest.test_case "the root task" `Quick test_root;
      Alcotest.test_case "within a task" `Quick test_within;
      Alcotest.test_case "two tasks" `Quick test_two_tasks;
      Alcotest.test_case "with_task" `Quick test_with_task;
      Alcotest.test_case "children" `Quick test_children;
      Alcotest.test_case "output" `Quick test_output;
      Alcotest.test_case "a label's style" `Quick test_label_style;
      Alcotest.test_case "a task's lines, muted" `Quick test_log_muted;
      Alcotest.test_case "a failed task's output, not muted" `Quick
        test_failure_not_muted;
      Alcotest.test_case "a task's lines, in the muted colour" `Quick
        test_log_muted_colour;
      Alcotest.test_case "bytes as rows show them" `Quick test_pp_bytes;
      Alcotest.test_case "durations as rows show them" `Quick test_pp_duration;
      Alcotest.test_case "advance" `Quick test_advance;
      Alcotest.test_case "a layout for each task" `Quick test_layout_choose;
      Alcotest.test_case "a transient child leaves" `Quick
        test_transient_child_leaves;
      Alcotest.test_case "rate" `Quick test_rate;
      Alcotest.test_case "a failed with_task says why" `Quick
        test_with_task_reason;
      Alcotest.test_case "iter" `Quick test_iter;
      Alcotest.test_case "a parent's mark is what the program says" `Quick
        test_parent_of_failure;
      Alcotest.test_case "a quiet failure names its parent" `Quick
        test_quiet_failure_context;
      Alcotest.test_case "a quiet transient failure shows" `Quick
        test_quiet_transient_failure;
      Alcotest.test_case "the summary runs between two tasks" `Quick
        test_summary_between_tasks;
      Alcotest.test_case "the 4 KiB cut keeps a flag whole" `Quick
        test_cap_keeps_a_flag;
      Alcotest.test_case "a terminal shows a transient failure" `Quick
        test_tty_transient_failure;
    ] )
