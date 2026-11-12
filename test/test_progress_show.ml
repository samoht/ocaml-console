(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness
open View_harness

let test_plain_lines () =
  let (), out =
    plain ~theme ~label:"Building" (fun d advance ->
        let t = Task.start d "compile" in
        Task.set_items t ~count:2 ~total:2;
        Task.log t "compiled parser.ml";
        advance 1.5;
        Task.log t "slow";
        Task.succeed t;
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
    (split out)

(* A line printed by the display starts at the first column, even when the
   program left a line unfinished on the same formatter. *)
let test_plain_column () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~now:(fun () -> 0.) ppf in
  Fmt.pf ppf "partial%!";
  Driver.report d `Info "next";
  Fmt.pf ppf "again";
  Task.succeed (Task.start d "t");
  Display.close display;
  Fmt.pf ppf "after@.";
  check_lines "each display line on its own"
    [ "partial"; "next"; "again"; "[-] t"; "[+] t  0.0s"; "after"; "" ]
    (String.split_on_char '\n' (Buffer.contents buffer))

(* A task still running when the root ends also ends. It is cancelled when
   the root is cancelled, and succeeds when the root succeeds. *)
let test_plain_unfinished () =
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Task.start d "left");
        Task.cancel d)
  in
  check_lines "cancelled"
    [ "[-] left"; "[~] left  0.0s  cancelled"; "" ]
    (split out);
  check_plain "succeeded with the work"
    (fun d _ ->
      ignore (Task.start d "left");
      Task.succeed ~message:"done" d)
    [ "[-] left"; "[+] left  0.0s"; "[+] done"; "" ]

(* Without a terminal, no colour shows a failure. So the last line of a task
   that did not succeed names its outcome, in the muted style of its time. *)
let test_plain_outcome_word () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.succeed (Task.start d "ok");
        let a = Task.start d "a" in
        Task.set_items a ~count:2 ~total:2;
        Task.fail a;
        Task.cancel (Task.start d "b"))
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
    (split out);
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `Ansi_tty;
  let d, display = show ~now:(fun () -> 0.) ppf in
  Task.fail (Task.start d "a");
  Display.close display;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "muted with the time" true
    (contains "0.0s  failed\027[" (Buffer.contents buffer))

let test_plain_attribution () =
  check_plain "each line says its task, in order"
    (fun d _ ->
      let a = Task.start d "a" and b = Task.start d "b" in
      Task.log a "one";
      Task.log b "two";
      Task.log a "three";
      Task.fail a;
      Task.cancel b)
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

(* A tab in a task's output moves to the next multiple of eight columns
   from the start of its line, so tabbed columns stay aligned. *)
let test_tabs () =
  check_plain "tab stops"
    (fun d _ ->
      let a = Task.start d "a" in
      Task.log a "x\ty\n\u{754c}\tz\n12345678\tw";
      Task.fail a)
    [
      "[-] a";
      "  # a: x       y";
      "  # a: \u{754c}      z";
      "  # a: 12345678        w";
      "[x] a  0.0s  failed";
      "";
    ]

let test_plain_sanitizes () =
  let (), out =
    plain ~label:"Build\027[2J" (fun d _ ->
        let task = Task.start d "a\tb" in
        Task.log task "x\007y";
        Task.succeed task)
  in
  Alcotest.(check bool) "no escape" false (String.contains out '\027');
  Alcotest.(check bool) "log text" true (contains "x y" out)

let test_plain_result () =
  let (), out =
    plain ~theme ~label:"Work" (fun d _ ->
        ignore (Task.start d "left running");
        Task.log d "see the log";
        Task.fail ~message:"it broke" d;
        Task.succeed ~message:"changed my mind" d)
  in
  check_lines "running tasks cancelled, verdict last"
    [
      "[-] left running";
      "see the log";
      "[~] left running  0.0s  cancelled";
      "[x] Work 0.0s (1/1)";
      "[x] it broke";
      "";
    ]
    (split out)

(* A task ended by its parent still shows how it ended, also on a pipe. *)
let test_nested_plain () =
  check_plain "a start line and an end line each"
    (fun d _ ->
      let p = Task.start d "parent" in
      let c1 = Task.start p "one" in
      ignore (Task.start p "two");
      Task.succeed c1;
      Task.fail p)
    [
      "[-] parent";
      "[-] parent \u{25B8} one";
      "[-] parent \u{25B8} two";
      "[+] parent \u{25B8} one  0.0s";
      "[~] parent \u{25B8} two  0.0s  cancelled";
      "[x] parent  0.0s  failed";
      "";
    ]

(* With [keep_ended], ended tasks stay also on a pipe, so the last frame
   sees them. *)
let test_keep_ended_plain () =
  let children = ref [] in
  let view root =
    View.fit (fun _ ->
        children :=
          List.concat_map
            (fun t -> List.map Task.label (Task.children t))
            (Task.children root);
        View.empty)
  in
  let (), _ =
    plain ~keep_ended:true ~view (fun root _ ->
        Task.run root "step" (fun step ->
            Task.succeed (Task.start step "layer")))
  in
  check_lines "the step's tasks, at the end" [ "layer" ] !children

(* On a pipe there is no window. A task prints a line when it starts, each
   of its lines at once after its label, and a line when it ends. *)
let test_pipe_lines () =
  check_plain "in turn, each after its task's label"
    (fun d _ ->
      Task.run d "job" (fun job ->
          Task.log job "one";
          Driver.report d `Info "note";
          Task.log job "two"))
    [
      "[-] job";
      "  # job: one";
      "  job: note";
      "  # job: two";
      "[+] job  0.0s";
      "";
    ]

(* On a pipe, [plain] gives the lines printed for task events. They replace
   the tasks' own lines and output. The root's output and the last frame
   print as before. A terminal ignores [plain]. *)
let test_plain_own_lines () =
  let plain e =
    match (Task.Event.kind e, Task.Event.task e) with
    | `End, Some (_, path) ->
        Some (View.text ("done " ^ String.concat "/" path))
    | _ -> None
  in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let root = Task.root ~label:"Build" ~now:(fun () -> 0.) () in
  let d = Progress.show ~progress:`Plain ~plain ppf root in
  Task.run root "job" (fun job ->
      Task.log job "one";
      Task.log root "above");
  Task.succeed root;
  Display.close d;
  Format.pp_print_flush ppf ();
  check_lines "the program's lines"
    [ "above"; "done job"; "[+] Build 0.0s (1/1)"; "" ]
    (split (Buffer.contents buffer));
  let t = terminal ~theme ~plain () in
  Task.succeed (Task.start t.root "job");
  close t;
  Alcotest.(check bool)
    "rows on a terminal" true
    (List.exists (contains "[+] job") (lines t))

(* The start line is written when the task starts, so a task that hangs
   leaves a trace. *)
let test_pipe_start () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~now:(fun () -> 0.) ppf in
  let p = Task.start d "fetch" in
  ignore (Task.start p "index");
  Format.pp_print_flush ppf ();
  check_lines "the start lines, at once"
    [ "[-] fetch"; "[-] fetch \u{25B8} index"; "" ]
    (split (Buffer.contents buffer));
  Display.close display

(* On a pipe nothing is kept. Every line was printed when it came, and none
   is printed again under the failure. *)
let test_failure_plain () =
  let n = 2000 in
  let (), out =
    plain ~theme (fun d _ ->
        let job = Task.start ~log_lines:2 d "job" in
        for i = 0 to n - 1 do
          Task.log job (Fmt.str "line%04d %s" i (String.make 600 'x'))
        done;
        Task.fail job)
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

(* Without a terminal, a view is drawn once, after the tasks' lines, when
   the display closes. *)
let test_view_plain () =
  let (), out =
    plain ~theme ~label:"Build"
      ~view:(above (View.text "TOP"))
      (fun d _ -> Task.succeed (Task.start d "compile"))
  in
  check_lines "the last frame, at the end"
    [ "[-] compile"; "[+] compile  0.0s"; "TOP"; "[+] Build 0.0s (1/1)"; "" ]
    (split out)

(* On a pipe, a task's output and its child tasks are told apart by their
   paths, not by indentation. A child's lines name its parent. A line of
   output names its own task, indented by two columns. *)
let test_plain_output_not_sibling () =
  check_plain "each line names its task"
    (fun d _ ->
      let p = Task.start d "parent" in
      let c = Task.start p "child" in
      Task.log p "parent says";
      Task.succeed c;
      Task.succeed p)
    [
      "[-] parent";
      "[-] parent \u{25B8} child";
      "  # parent: parent says";
      "[+] parent \u{25B8} child  0.0s";
      "[+] parent  0.0s";
      "";
    ]

(* On a pipe the output of a failure is printed once. The lines streamed as
   they came are not printed again under the failure. *)
let test_plain_failure_output_once () =
  let (), out =
    plain ~theme (fun d _ ->
        let job = Task.start d "job" in
        Task.log job "checksum mismatch";
        Task.fail job)
  in
  Alcotest.(check int)
    "the line once" 1
    (List.length (List.filter (contains "checksum mismatch") (split out)))

(* On a pipe, the lines of two trees that run at once are interleaved. A
   child's line names its parent, as output lines name their task. *)
let test_plain_interleaved_children () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start d "linux" and b = Task.start d "macos" in
        let fa = Task.start a "fetch" in
        let fb = Task.start b "fetch" in
        Task.succeed fa;
        Task.succeed fb;
        Task.succeed a;
        Task.succeed b)
  in
  let fetches = List.filter (contains "fetch") (split out) in
  Alcotest.(check bool)
    "each fetch line names its build" true
    (List.for_all (fun l -> contains "linux" l || contains "macos" l) fetches)

(* The mark of an output line is not the path separator. *)
let test_pipe_output_mark_not_separator () =
  let (), out =
    plain (fun d _ ->
        let a = Task.start d "a" in
        let b = Task.start a "b" in
        Task.log b "x";
        Task.succeed b;
        Task.succeed a)
  in
  let line = List.find (contains ": x") (split out) in
  let count = List.length (Re.matches (Re.Perl.compile_pat "\u{203A}") line) in
  Alcotest.(check int) ("one U+203A in " ^ String.escaped line) 1 count

(* On a pipe, a failed transient task prints its line and its reason. *)
let test_plain_transient_failure () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~label:"S" ~progress:`Plain ~now:(fun () -> 0.) ppf in
  (try Task.run ~transient:true d "probe" (fun _ -> failwith "disk full")
   with Failure _ -> ());
  Display.close display;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buffer in
  Alcotest.(check bool)
    ("the failure shows in\n" ^ out)
    true (contains "probe" out)

(* With [`Plain], the last frame is as wide as the terminal if there is one,
   and is not cut if there is none. *)
let test_plain_last_frame_width () =
  let rule = View.fit (fun _ -> Rule.view ()) in
  let last ?size () =
    let buffer = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buffer in
    let _, display =
      show ~progress:`Plain ?size ~view:(Fun.const rule) ~now:(fun () -> 0.) ppf
    in
    Display.close display;
    Format.pp_print_flush ppf ();
    let lines = String.split_on_char '\n' (Buffer.contents buffer) in
    List.fold_left (fun n l -> max n (View.width (View.text l))) 0 lines
  in
  Alcotest.(check int)
    "a terminal of 40 columns" 40
    (last ~size:(fun () -> (40, 24)) ());
  Alcotest.(check int) "whole on a pipe" 4 (last ())

(* When a view has a table taller than the space, the table loses its first
   lines. The tasks and the summary stay. *)
let test_tall_header () =
  let t = terminal ~width:40 ~height:8 ~label:"S" () in
  let rows =
    View.vcat (List.init 20 (fun i -> Fmt.kstr View.text "row %d" i))
  in
  Display.set_view t.display (above rows t.root);
  let a = Task.start t.root "a" in
  Display.redraw t.display;
  let shown = List.map untimed (screen t) in
  Alcotest.(check int) "the screen less one" 7 (List.length shown);
  check_lines "the table's last lines"
    [ "row 15"; "row 16"; "row 17"; "row 18"; "row 19" ]
    (List.filteri (fun i _ -> i < 5) shown);
  Alcotest.(check bool) "the task" true (contains " a" (List.nth shown 5));
  Alcotest.(check bool) "the summary" true (contains "S" (List.nth shown 6));
  Task.succeed a;
  close t

let steps =
  Table.view
    [
      Table.column (View.text "step");
      Table.column ~align:`Right (View.text "time");
    ]
    [
      [ View.text "fetch"; View.text "1.2s" ];
      [ View.text "compile"; View.text "3.4s" ];
    ]

(* [printed ~margin pp v] is the lines that [pp] prints for [v] on a
   formatter with [theme] and [margin]. *)
let printed ?(theme = theme) ~margin pp v =
  split
    ( capture @@ fun ppf ->
      Theme.set ppf theme;
      Format.pp_set_margin ppf margin;
      pp ppf v )

let table ~width = View.sized ~width:(min width 100) steps

(* A view set on the display fills the region. Here it is a table above the
   default view: the tree of the tasks, then the summary on the last line.
   The view gets the terminal's width and its height minus one. *)
let test_view () =
  let t = terminal ~theme ~width:40 ~height:12 ~label:"Build" () in
  let heights = ref [] in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         heights := height :: !heights;
         above (table ~width) d));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let table = printed ~margin:40 View.pp steps in
  let rows = screen t in
  let n = List.length table in
  check_lines "the table first" table (List.filteri (fun i _ -> i < n) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows n));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows (n + 1)));
  Alcotest.(check int) "the room" 11 (List.hd !heights)

(* A view line wider than the terminal is cut. A view taller than the space
   loses its first lines, so the summary at its bottom stays. *)
let test_view_fits () =
  let t = terminal ~theme ~width:20 ~height:6 ~label:"Build" () in
  let tall = View.text "a view line much wider than twenty\nb\nc\nd\ne\nf" in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat [ tall; default d ~width ~height:2 ]));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let rows = screen t in
  check_lines "the last five lines" [ "d"; "e"; "f" ]
    (List.filteri (fun i _ -> i < 3) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows 3));
  Alcotest.(check bool)
    "the summary" true
    (contains "[-] Build" (List.nth rows 4));
  Alcotest.(check int) "within the screen less one" 5 (List.length rows);
  Display.set_view t.display (View.text "a view line much wider than twenty\nb");
  check_lines "cut to the width"
    [ "a view line much wi\u{2026}"; "b" ]
    (screen t);
  close t

(* On a terminal too short for the view, the view loses its first lines. A
   table that would be cut in height is dropped whole, also next to text in
   an [hcat], so the region shows no part of its frame. *)
let test_view_short () =
  let t = terminal ~theme ~width:40 ~height:6 ~label:"Build" () in
  let table = View.sized ~width:30 steps in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat [ table; default d ~width ~height:2 ]));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check int) "the tree and the summary" 2 (List.length rows);
  Alcotest.(check bool)
    "nothing of the table" false
    (List.exists (fun r -> contains "+-" r || contains "s |" r) rows);
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat
           [
             View.text "a\nb\nc";
             View.hcat [ table; View.text "x" ];
             View.text "d\ne";
             default d ~width ~height:2;
           ]));
  check_lines "past the table, the text" [ "d"; "e" ]
    (List.filteri (fun i _ -> i < 2) (screen t));
  close t

(* On a wide terminal the view gets the full width, and the tree uses at
   most 100 columns. *)
let test_view_width () =
  let t = terminal ~theme ~width:160 () in
  let given = ref 0 in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         given := width;
         default d ~width ~height));
  Task.set_items
    (Task.start t.root ("compile " ^ String.make 62 'x'))
    ~count:1 ~total:3;
  Display.redraw t.display;
  Alcotest.(check int) "the width" 160 !given;
  Alcotest.(check int)
    "the row" 100
    (Console_width.string_width (List.hd (screen t)))

(* The default view and the summary use the width they are given, so a view
   can put them in a column narrower than the terminal. *)
let test_default_view_column () =
  let t = terminal ~theme ~width:120 ~height:12 ~label:"Building" () in
  let panel = View.sized ~width:30 (Panel.view (View.text "status")) in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         View.hcat [ default d ~width:(width - 32) ~height; panel ]));
  let step = Task.start t.root "compile" in
  Task.set_items step ~count:1 ~total:2;
  Display.redraw t.display;
  let view = default t.root ~width:30 ~height:10 in
  Alcotest.(check bool) "the view in 30 cells" true (View.width view <= 30);
  Alcotest.(check bool)
    "the summary in 12" true
    (View.width (View.sized ~width:12 (Progress.summary t.root)) <= 12);
  Alcotest.(check bool)
    "the panel on the screen" true
    (List.for_all (fun r -> Console_width.string_width r <= 120) (screen t)
    && List.exists (contains "status") (screen t));
  close t

(* Widgets side by side make a view; each one uses the width it is given. *)
let test_view_hcat () =
  let t = terminal ~theme ~width:40 ~height:12 ~label:"Build" () in
  let panel = Panel.view (View.text "status") in
  let side width =
    let half = (width - 2) / 2 in
    View.hcat [ View.sized ~width:half steps; View.sized ~width:half panel ]
  in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ -> above (side width) d));
  Display.redraw t.display;
  let want = printed ~margin:40 View.pp (side 40) in
  let rows = screen t in
  check_lines "side by side" want
    (List.filteri (fun i _ -> i < List.length want) rows);
  Alcotest.(check bool)
    "on one line" true
    (contains "step" (List.nth rows 1) && contains "status" (List.nth rows 1))

(* A new view replaces the previous one. Its last frame stays after the
   display closes. *)
let test_view_replaced () =
  let t = terminal ~theme ~width:30 () in
  Display.set_view t.display (View.text "first");
  Display.redraw t.display;
  Display.set_view t.display (View.text "second");
  check_lines "the second view, at once" [ "second" ] (screen t);
  close t;
  check_lines "its last frame stays" [ "second" ] (lines t)

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

let clusters = Console_width.clusters

(* In a live display with a motion theme, a table's border moves. From one
   frame to the next, only the border cells change, each to a glyph of the
   same width. So every line keeps its width and every cell its place. *)
let test_view_moves () =
  let run theme ~styled =
    let t = terminal ~theme ~styled ~width:40 ~height:12 () in
    Display.set_view t.display
      (fit t.root (fun _ ~width ~height:_ -> table ~width));
    Display.redraw t.display;
    let frames =
      List.init 12 (fun _ ->
          let before = writes t in
          t.advance 0.25;
          Display.redraw t.display;
          (writes t > before, screen t))
    in
    close t;
    frames
  in
  let still = printed ~theme:Theme.matrix ~margin:40 View.pp steps in
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
                     && Console_width.string_width a
                        = Console_width.string_width b)
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

(* A table in [Border.simple] has no frame to move. In a motion theme, every
   frame of the view is the still frame. *)
let test_simple_view_stands_still () =
  let simple =
    Table.view ~border:Border.simple
      [
        Table.column (View.text "step");
        Table.column ~align:`Right (View.text "time");
      ]
      [
        [ View.text "fetch"; View.text "1.2s" ];
        [ View.text "compile"; View.text "3.4s" ];
      ]
  in
  List.iter
    (fun theme ->
      let t = terminal ~theme ~styled:true ~width:40 ~height:12 () in
      Display.set_view t.display simple;
      Display.redraw t.display;
      let still = screen t in
      for _ = 1 to 12 do
        t.advance 0.25;
        Display.redraw t.display;
        Alcotest.(check (list string)) "still" still (screen t)
      done;
      close t)
    Theme.[ matrix; rainbow; tokyo_night ]

let suite =
  ( "progress_show",
    [
      Alcotest.test_case "plain lines" `Quick test_plain_lines;
      Alcotest.test_case "plain lines of the program" `Quick
        test_plain_own_lines;
      Alcotest.test_case "plain attribution" `Quick test_plain_attribution;
      Alcotest.test_case "plain unfinished tasks" `Quick test_plain_unfinished;
      Alcotest.test_case "a plain closing line says how it ended" `Quick
        test_plain_outcome_word;
      Alcotest.test_case "plain lines start a line" `Quick test_plain_column;
      Alcotest.test_case "plain sanitizes" `Quick test_plain_sanitizes;
      Alcotest.test_case "plain result" `Quick test_plain_result;
      Alcotest.test_case "nested on a pipe" `Quick test_nested_plain;
      Alcotest.test_case "tab stops in a task's output" `Quick test_tabs;
      Alcotest.test_case "pipe lines" `Quick test_pipe_lines;
      Alcotest.test_case "pipe start lines" `Quick test_pipe_start;
      Alcotest.test_case "a failure on a pipe" `Quick test_failure_plain;
      Alcotest.test_case "keep_ended on a pipe" `Quick test_keep_ended_plain;
      Alcotest.test_case "view on a pipe" `Quick test_view_plain;
      Alcotest.test_case "a pipe's output is not a sibling of the children"
        `Quick test_plain_output_not_sibling;
      Alcotest.test_case "a pipe prints a failure's output once" `Quick
        test_plain_failure_output_once;
      Alcotest.test_case "a pipe names the parent of an interleaved child"
        `Quick test_plain_interleaved_children;
      Alcotest.test_case "a pipe's output mark is not the path separator" `Quick
        test_pipe_output_mark_not_separator;
      Alcotest.test_case "a pipe shows a transient failure" `Quick
        test_plain_transient_failure;
      Alcotest.test_case "plain's last frame is the terminal's width" `Quick
        test_plain_last_frame_width;
      Alcotest.test_case "a view side by side" `Quick test_view_hcat;
      Alcotest.test_case "a simple table stands still" `Quick
        test_simple_view_stands_still;
      Alcotest.test_case "a view taller than the room" `Quick test_tall_header;
      Alcotest.test_case "view" `Quick test_view;
      Alcotest.test_case "view fits" `Quick test_view_fits;
      Alcotest.test_case "view width" `Quick test_view_width;
      Alcotest.test_case "a view on a short terminal" `Quick test_view_short;
      Alcotest.test_case "the default view in a column" `Quick
        test_default_view_column;
      Alcotest.test_case "view replaced" `Quick test_view_replaced;
      Alcotest.test_case "view moves" `Quick test_view_moves;
    ] )
