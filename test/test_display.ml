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
  Theme.set ppf theme;
  let d = Display.v ~now:(fun () -> 0.) ppf in
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
  check_lines "nothing left once closed" [] (lines t);
  check_invalid "Console.Display.set_scene: closed display" (fun () ->
      Display.set_scene t.display (fun ~height:_ _ -> ()))

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
      Alcotest.test_case "the marker counts" `Quick test_marker_counts;
      Alcotest.test_case "with_output" `Quick test_with_output;
      Alcotest.test_case "with_output without a line end" `Quick
        test_with_output_partial;
      Alcotest.test_case "idle redraw writes nothing" `Quick test_redraw_idle;
      Alcotest.test_case "narrowing" `Quick test_narrowing;
      Alcotest.test_case "wide" `Quick test_wide;
      Alcotest.test_case "synchronized" `Quick test_synchronized;
      Alcotest.test_case "height changes" `Quick test_height_changes;
      Alcotest.test_case "nested" `Quick test_nested;
      Alcotest.test_case "keys and transient" `Quick test_keys_and_transient;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "scene" `Quick test_scene;
      Alcotest.test_case "scene fits" `Quick test_scene_fits;
      Alcotest.test_case "scene width" `Quick test_scene_width;
      Alcotest.test_case "scene replaced" `Quick test_scene_replaced;
      Alcotest.test_case "scene on a pipe" `Quick test_scene_plain;
      Alcotest.test_case "scene moves" `Quick test_scene_moves;
    ] )
