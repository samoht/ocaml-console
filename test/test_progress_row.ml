(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* [cell_of needle row] is the cell where [needle] starts in [row]. *)
let cell_of needle row =
  let rec find i =
    if i + String.length needle > String.length row then
      Alcotest.failf "%S is not in %S" needle row
    else if String.sub row i (String.length needle) = needle then i
    else find (i + 1)
  in
  View.width (View.text (String.sub row 0 (find 0)))

let test_alignment () =
  let t = terminal ~theme ~width:40 () in
  let a = Task.start t.root "\xe7\x95\x8c\xe7\x95\x8c wide" in
  let b =
    Task.start t.root "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9 emoji"
  in
  let c = Task.start t.root "plain" in
  Task.set_items a ~count:1 ~total:10;
  Task.set_items b ~count:10 ~total:100;
  Task.set_items c ~count:3 ~total:3;
  List.iter Task.succeed [ a; b; c ];
  close t;
  let rows = lines t in
  let one msg cols =
    Alcotest.(check int)
      (msg ^ " in\n" ^ String.concat "\n" rows)
      1
      (List.length (List.sort_uniq Int.compare cols))
  in
  one "counts end in one column"
    (List.map2
       (fun n r -> cell_of n r + String.length n)
       [ "1/10"; "10/100"; "3/3" ]
       rows);
  one "timers end in one column" (List.map (fun r -> cell_of "0.0s" r + 4) rows)

let test_label_truncated () =
  let t = terminal ~theme ~width:20 () in
  ignore (Task.start t.root "a label much too long for the row");
  Display.redraw t.display;
  let row = List.nth (screen t) 0 in
  Alcotest.(check bool) "ellipsis" true (contains "\xe2\x80\xa6" row);
  Alcotest.(check bool) "fits" true (View.width (View.text row) <= 20)

(* A running bar is in the accent colour. Once its task ends, the bar is
   muted for a success and in the failure colour for a failure. *)
let test_ended_bar_colour () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let drawn task =
    View.to_string ~styled:true (View.sized ~width:40 (Progress.row task))
  in
  let job count =
    let task = Task.start root "job" in
    Task.set_items task ~count ~total:4;
    task
  in
  let running = job 4 and passed = job 4 and failed = job 2 in
  Task.succeed passed;
  Task.fail failed;
  let has msg sgr task =
    Alcotest.(check bool) msg true (contains (sgr ^ "\u{2501}") (drawn task))
  in
  has "running, in the accent" "\027[94m" running;
  has "a success, muted" "\027[2m" passed;
  has "a failure, in red" "\027[31m" failed

(* A count of bytes is never wider than "1023.99 KiB". The count and rate
   columns open at that width. *)
let test_bytes_width () =
  let widest = String.length "1023.99 KiB" in
  let check n =
    let s = Fmt.str "%a" Progress.pp_bytes n in
    if String.length s > widest then Alcotest.failf "%Ld is %S" n s
  in
  List.iter check [ Int64.max_int; Int64.min_int; 0L ];
  for unit = 0 to 6 do
    let base = Int64.shift_left 1L (10 * unit) in
    for k = 1 to 1024 do
      let n = Int64.mul base (Int64.of_int k) in
      List.iter
        (fun d -> if Int64.compare n 0L > 0 then check (Int64.add n d))
        [ -1L; 0L; 1L; Int64.neg (Int64.div n 2000L) ]
    done
  done

(* The label column measures only the labels that rows draw. A running row
   with no label in its layout does not widen it. *)
let test_label_column_drawn () =
  let layout =
    Progress.Layout.(
      choose (fun task ->
          if Task.label task = "unlabelled and long" then
            const (View.text "bytes")
          else list [ label (); const (View.text " |") ]))
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let a = Task.start t.root "short" in
  let b = Task.start t.root "unlabelled and long" in
  Display.redraw t.display;
  check_lines "as wide as the drawn labels" [ "short |"; "bytes" ] (screen t);
  Task.succeed a;
  Task.succeed b;
  close t

(* On a narrow terminal the label is what tells two rows apart. It is cut
   only after the bar is dropped, and the row still ends with its timer. *)
let test_label_before_bar () =
  let t = terminal ~theme ~width:40 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
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
  close t

(* The same, with two trees side by side on 75 columns, as in
   examples/columns. *)
let test_label_before_bar_hcat () =
  let t = terminal ~theme ~width:75 ~height:12 () in
  let view d ~width ~height =
    let half = (min width 100 - 2) / 2 in
    match Task.children d with
    | [ a; b ] ->
        View.hcat
          [
            View.sized ~width:half ~height (Progress.tree a);
            View.sized ~width:half ~height (Progress.tree b);
          ]
    | _ -> default d ~width ~height
  in
  Display.set_view t.display (fit t.root view);
  let a = Task.start t.root "linux" and b = Task.start t.root "macos" in
  let ca = Task.start a "compile" in
  let cb = Task.start b "compile" in
  Task.set_items ca ~count:4 ~total:12;
  Task.set_items cb ~count:2 ~total:12;
  Display.redraw t.display;
  Alcotest.(check bool)
    "compile shows in a 36-cell tree" true
    (List.exists (contains "comp") (screen t));
  close t

(* When the display closes, each task's label stays readable in the
   scrollback. A label that does not fit next to its count continues below,
   never one character per line. *)
let test_last_frame_label_readable () =
  let t = terminal ~theme ~width:40 ~height:30 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd..." in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:2147483648L ~total:2147483648L;
  Task.succeed layer;
  Task.succeed step;
  close t;
  let rows = lines t in
  Alcotest.(check bool)
    "no row holds a lone character" false
    (List.exists (fun r -> String.length (String.trim r) = 1) rows);
  Alcotest.(check bool)
    "each label on one row" true
    (List.exists (contains "RUN make") rows
    && List.exists (contains "sha256") rows)

(* A label is not cut to make room for a count column that its row leaves
   empty. *)
let test_label_not_cut_for_an_empty_count () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_utf_8 ppf true;
  let d, _display =
    show ~size:(fun () -> (40, 30)) ~progress:`Tty ~now:(fun () -> 0.) ppf
  in
  let rec nest p n =
    if n = 0 then p else nest (Task.start p (Fmt.str "level-%d-label" n)) (n - 1)
  in
  let leaf = nest d 8 in
  Task.set_items leaf ~count:1 ~total:4;
  let tree =
    View.to_string (View.sized ~width:40 ~height:30 (Progress.tree d))
  in
  Alcotest.(check bool)
    ("level-5-label whole in\n" ^ tree)
    true
    (contains "level-5-label" tree)

(* [bytes_row_at width] is the row of a task counting bytes, with a label
   of 30 cells, after 5 seconds, on a terminal [width] wide. *)
let bytes_row_at width =
  let t = terminal ~theme ~width ~height:4 () in
  let r = Task.start t.root "a-label-of-thirty-cells-long-x" in
  t.advance 1.;
  Task.set_bytes r ~count:268435456L ~total:2147483648L;
  t.advance 4.;
  Display.redraw t.display;
  let row = List.hd (screen t) in
  close t;
  row

(* Widths go from 80 cells down to 20. The row of bytes keeps:
   - its whole label down to 67 cells (3 + 1 + 30 + 1 + 25 + 1 + 6);
   - its bar down to 78 (the narrowest bar is 10 cells, plus one blank);
   - its rate never, as the rate needs 92;
   - its full count down to 49, where the label is cut to 12 cells;
   - its time down to 33, where the short count of 9 cells and a label of
     12 still fit. *)
let test_count_and_time_after_label () =
  let bar = Re.Perl.compile_pat "\u{2588}|\u{2591}" in
  for width = 80 downto 20 do
    let row = bytes_row_at width in
    let shows what expected actual =
      Alcotest.(check bool)
        (Fmt.str "at %d, %s in %S" width what row)
        expected actual
    in
    shows "the whole label" (width >= 67)
      (contains "a-label-of-thirty-cells-long-x" row);
    shows "the bar" (width >= 78) (Re.execp bar row);
    shows "the rate" false (contains "/s" row);
    shows "the full count" (width >= 49) (contains "256.00 MiB / 2.00 GiB" row);
    shows "the short count" (width < 49) (contains "256M/2.0G" row);
    shows "the time" (width >= 33) (contains "5.0s" row)
  done

(* [bar_end row] is the cell of the last block or shade glyph in [row]. *)
let bar_end row = Option.map snd (bar_span row)

let test_bars_line_up () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:2 ~total:5;
  t.advance 1.;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
  t.advance 1.;
  Task.set_bytes layer ~count:536870912L ~total:2147483648L;
  Display.redraw t.display;
  let rows = screen t in
  let ends = List.filter_map bar_end rows in
  Alcotest.(check int)
    ("two bars in\n" ^ String.concat "\n" rows)
    2 (List.length ends);
  Alcotest.(check bool)
    ("the bars end in one column in\n" ^ String.concat "\n" rows)
    true
    (List.for_all (( = ) (List.hd ends)) ends);
  close t

(* [cells row] is the width of [row] in cells, counting one cell for each
   glyph. *)
let cells row = Array.length (glyphs row)
let span = Alcotest.(option (pair int int))

(* A row is packed from the left: the label, the bar, the count and the
   time. The bars of all rows start one cell after the widest label and have
   the same width, at most 40 cells. The rest of the line stays empty. *)
let test_rows_pack_left () =
  let t = terminal ~theme ~width:100 ~height:8 () in
  let labels = [ "parser"; "layout"; "a-longer-render-suite" ] in
  List.iteri
    (fun i l -> Task.set_items (Task.start t.root l) ~count:(10 + i) ~total:120)
    labels;
  Display.redraw t.display;
  let rows = screen t in
  let all = String.concat "\n" rows in
  let row l = List.find (contains l) rows in
  match List.map (fun l -> bar_span (row l)) labels with
  | Some (first, last) :: _ as spans ->
      List.iter
        (Alcotest.check span
           ("the bars in one column in\n" ^ all)
           (Some (first, last)))
        spans;
      Alcotest.(check int) ("bars of 40 cells in\n" ^ all) 40 (last - first + 1);
      let widest =
        List.fold_left
          (fun n l -> max n (cell_of l (row l) + String.length l))
          0 labels
      in
      Alcotest.(check int)
        ("one cell between the widest label and the bars in\n" ^ all)
        (widest + 1) first;
      List.iter
        (fun l ->
          Alcotest.(check bool)
            ("the end of the line empty in\n" ^ all)
            true
            (cells (row l) < 100))
        labels;
      close t
  | _ -> Alcotest.fail ("no bar in\n" ^ all)

(* As the terminal narrows, the bar loses one cell for each lost column, from
   40 cells down to 10. Only then is the rate dropped. *)
let test_bar_shrinks_first () =
  let row_at width =
    let t = terminal ~theme ~width ~height:4 () in
    let r = Task.start t.root "layer" in
    t.advance 1.;
    Task.set_bytes r ~count:268435456L ~total:2147483648L;
    t.advance 1.;
    Task.set_bytes r ~count:536870912L ~total:2147483648L;
    Display.redraw t.display;
    let row = List.find (contains "layer") (screen t) in
    close t;
    ( Option.map (fun (first, last) -> last - first + 1) (bar_span row),
      contains "/s" row,
      row )
  in
  let rows = List.init 71 (fun i -> row_at (110 - i)) in
  (match rows with
  | (bar, _, row) :: _ ->
      Alcotest.(check (option int))
        ("a bar of 40 cells in\n" ^ row)
        (Some 40) bar
  | [] -> ());
  let rec walk = function
    | (bar, true, row) :: ((bar', rate', row') :: _ as rest) ->
        let msg = Fmt.str "from\n%s\nto\n%s" row row' in
        (match (bar, bar') with
        | Some b, Some b' when rate' ->
            Alcotest.(check bool)
              ("the bar loses at most a cell " ^ msg)
              true
              (b' = b - 1 || (b = 40 && b' = 40))
        | Some b, _ ->
            Alcotest.(check int) ("a bar of 10 cells, then no rate " ^ msg) 10 b
        | None, _ -> Alcotest.fail ("a rate without a bar " ^ msg));
        walk rest
    | (_, false, row) :: (_, true, row') :: _ ->
        Alcotest.failf "the rate comes back from\n%s\nto\n%s" row row'
    | _ :: rest -> walk rest
    | [] -> ()
  in
  walk rows;
  Alcotest.(check bool)
    "the rate gives way" true
    (List.exists (fun (_, rate, _) -> not rate) rows)

(* A shortened count of bytes has at most three digits: 1000 of a unit or
   more is shown in the next unit. *)
let test_short_bytes () =
  let row_at width =
    let t = terminal ~theme ~width ~height:4 () in
    let r = Task.start t.root "layer" in
    Task.set_bytes r ~count:1048575L ~total:1048576000L;
    Display.redraw t.display;
    let row = List.find (contains "layer") (screen t) in
    close t;
    row
  in
  let rows = List.init 40 (fun i -> row_at (20 + i)) in
  Alcotest.(check bool)
    ("1.0M/1.0G in\n" ^ String.concat "\n" rows)
    true
    (List.exists (contains "1.0M/1.0G") rows);
  List.iter
    (fun row ->
      Alcotest.(check bool)
        ("no four digits in\n" ^ row)
        false
        (contains "1024K" row || contains "1000M" row))
    rows

(* [full row] is [true] iff the bar of [row] has no cell of its track. *)
let full row =
  match bar_span row with
  | None -> false
  | Some (i, j) ->
      Array.for_all
        (Uchar.equal (Uchar.of_int 0x2588))
        (Array.sub (glyphs row) i (j - i + 1))

(* A task that ends keeps its count and time in the same columns as the
   running rows, and its full bar in the same cells as their bars. *)
let test_ended_keeps_columns () =
  let t = terminal ~theme ~width:100 ~height:8 () in
  let build = Task.start t.root "build" in
  let a = Task.start build "running" and b = Task.start build "ends" in
  Task.set_items a ~count:3 ~total:12;
  Task.set_items b ~count:12 ~total:12;
  Display.redraw t.display;
  let before = screen t in
  Task.succeed b;
  Display.redraw t.display;
  let after = screen t in
  let row l rows = List.find (contains l) rows in
  let msg =
    "in\n" ^ String.concat "\n" before ^ "\nthen\n" ^ String.concat "\n" after
  in
  let count rows = cell_of "12/12" (row "ends" rows) + 5 in
  Alcotest.(check int)
    ("the count of the task that ended stays " ^ msg)
    (count before) (count after);
  Alcotest.(check int)
    ("in the column of the counts " ^ msg)
    (cell_of "3/12" (row "running" after) + 4)
    (count after);
  Alcotest.(check int)
    ("its time ends in its column " ^ msg)
    (cells (row "running" after))
    (cells (row "ends" after));
  Alcotest.check span
    ("its bar in the cells of theirs " ^ msg)
    (bar_span (row "running" after))
    (bar_span (row "ends" after));
  Alcotest.(check bool) ("its bar full " ^ msg) true (full (row "ends" after));
  close t

(* When the bar is dropped, the count and the time keep their columns,
   right-aligned and padded. On 40 columns, a row with a 16-cell label, a
   count and a time takes 35 cells; with the narrowest bar it takes 46. *)
let test_count_and_time_padded () =
  let t = terminal ~theme ~width:40 ~height:8 () in
  let rows =
    [
      ("pins synced", 24, 275);
      ("opam synced", 515, 515);
      ("packages checked", 515, 515);
    ]
  in
  List.iter
    (fun (l, count, total) ->
      Task.set_items (Task.start t.root l) ~count ~total)
    rows;
  t.advance 1.6;
  Display.redraw t.display;
  check_lines "the count and the time in their columns"
    [
      " |  pins synced       24/275   1.6s";
      " |  opam synced      515/515   1.6s";
      " |  packages checked 515/515   1.6s";
    ]
    (screen t);
  close t

(* A sync in the style of monopam, with a label column fixed at 16 cells.
   The first step is briefly renamed to a label of 66 cells, which is cut to
   the column. No row moves sideways, and the steps move to the scrollback in
   that column. *)
let test_replaced_label_widens_nothing () =
  let layout =
    Progress.Layout.(
      list
        ~sep:(const (View.text " "))
        [ spinner; label ~width:16 (); bar (); count; rate; elapsed ])
  in
  let t = terminal ~theme ~width:40 ~height:8 ~layout () in
  let long =
    "asn1-combinators.72f5a97d25b0dce59820f089b61908d2fd742072 complete"
  in
  let step ?relabel label ~message ~count ~total =
    let r = Task.start t.root label in
    Option.iter (Task.set_label r) relabel;
    Task.set_items r ~count ~total;
    t.advance 0.3;
    Display.redraw t.display;
    let live = screen t in
    Task.succeed ~message r;
    live
  in
  let live =
    step ~relabel:long "syncing pins" ~message:"pins synced" ~count:24
      ~total:275
  in
  check_lines "the long label cut to the column"
    [ " \\  asn1-combinator\u{2026}  24/275   0.3s" ]
    live;
  let live = step "syncing opam" ~message:"opam synced" ~count:515 ~total:515 in
  check_lines "the column fixed"
    [
      "[+] pins synced       24/275   0.3s";
      " -  syncing opam     515/515   0.3s";
    ]
    live;
  ignore (step "checking" ~message:"packages checked" ~count:515 ~total:515);
  close t;
  check_lines "the steps in that column"
    [
      "[+] pins synced       24/275   0.3s";
      "[+] opam synced      515/515   0.3s";
      "[+] packages checked 515/515   0.3s";
    ]
    (lines t)

(* The label column keeps the widest label a running task has drawn. So a
   task renamed at each step moves no row sideways. The column keeps that
   width after the task ends. *)
let test_running_label_kept () =
  let t = terminal ~theme ~width:40 ~height:8 ~keep_ended:true () in
  let a = Task.start t.root "a" and b = Task.start t.root "b" in
  Task.set_items b ~count:1 ~total:9;
  Task.set_label a "a-longer-label";
  Display.redraw t.display;
  Task.set_label a "a";
  Display.redraw t.display;
  check_lines "the column of the widest label a drew"
    [ " |  a                        0.0s"; " |  b                  1/9   0.0s" ]
    (screen t);
  Task.succeed ~message:"done" a;
  Display.redraw t.display;
  check_lines "the column does not narrow"
    [ "[+] done                     0.0s"; " |  b                  1/9   0.0s" ]
    (screen t);
  Task.succeed b;
  close t

(* A line printed while a task with a wider label runs first moves the
   waiting tasks, in that label's column. The rows printed after them keep
   that width, even though the wider task then ends with a shorter
   message. *)
let test_printed_width_kept () =
  let t = terminal ~theme ~width:40 ~height:8 () in
  let fetch = Task.start t.root "fetch dependencies" in
  let build = Task.start t.root "build" in
  Display.redraw t.display;
  Task.succeed build;
  Display.print t.display (View.text "note");
  Task.succeed ~message:"fetched" fetch;
  close t;
  check_lines "one label column"
    [ "[+] build                0.0s"; "note"; "[+] fetched              0.0s" ]
    (lines t)

(* Steps that end one after another wait in the region. They move to the
   scrollback together, in one label column and one count column, even
   though the first label is the shortest and a later count the widest. *)
let test_steps_share_columns () =
  let t = terminal ~theme ~width:60 ~height:12 () in
  let bar n = String.concat "" (List.init n (fun _ -> "\u{2588}")) in
  let step label ~message total =
    let r = Task.start t.root label in
    Task.set_items r ~count:total ~total;
    Display.redraw t.display;
    Task.succeed ~message r
  in
  step "indexing" ~message:"indexed" 518;
  step "discovering packages" ~message:"packages discovered" 517;
  step "splitting" ~message:"split" 42331;
  step "exporting" ~message:"exported" 98;
  Alcotest.(check int)
    "nothing in the scrollback yet" 0
    (Console_vte.scrollback_size t.vte);
  Display.print t.display (View.text "done");
  close t;
  check_lines "one column each"
    [
      "[+] indexed              " ^ bar 16 ^ "     518/518   0.0s";
      "[+] packages discovered  " ^ bar 16 ^ "     517/517   0.0s";
      "[+] split                " ^ bar 16 ^ " 42331/42331   0.0s";
      "[+] exported             " ^ bar 16 ^ "       98/98   0.0s";
      "done";
    ]
    (lines t)

(* When the region is too short for the waiting tasks, the oldest of them
   move out first, in the order they ended. *)
let test_held_tasks_fit () =
  let t = terminal ~theme ~width:30 ~height:4 () in
  List.iter
    (fun l ->
      Task.succeed (Task.start t.root l);
      Display.redraw t.display)
    [ "a"; "b"; "c"; "d"; "e" ];
  check_lines "the oldest moved, none folded"
    [ "[+] a   0.0s"; "[+] b   0.0s" ]
    (List.filteri (fun i _ -> i < 2) (lines t));
  close t;
  check_lines "each task once, in order"
    [
      "[+] a   0.0s";
      "[+] b   0.0s";
      "[+] c   0.0s";
      "[+] d   0.0s";
      "[+] e   0.0s";
    ]
    (lines t)

(* A row of bytes with a label of 30 cells, as the terminal narrows:
   - the rate is dropped, then the bar;
   - the label is cut down to 12 cells, and the count and the time keep
     their columns;
   - the count gets shorter, in its own column;
   - the time is dropped;
   - only then is the label cut further. *)
let test_label_cut_before_count () =
  check_lines "each width"
    [
      " -  a-label-of-thirty-cells-long-x     256.00 MiB / 2.00 GiB   5.0s";
      " -  a-label-of-thirty-cell\u{2026}     256.00 MiB / 2.00 GiB   5.0s";
      " -  a-label-of-thirty-cells-lo\u{2026} 256M/2.0G   5.0s";
      " -  a-label-of-thirty\u{2026} 256M/2.0G";
      " -  a-lab\u{2026} 256M/2.0G";
    ]
    (List.map bytes_row_at [ 70; 60; 48; 32; 20 ])

(* The line of a task that ended at its total has no gap between its label
   and its count: its bar is full. A task that failed before its total draws
   its bar as far as it got. *)
let test_ended_bar () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let total = 60 in
  let render = Task.start t.root "render" and lint = Task.start t.root "lint" in
  for count = 1 to total do
    Task.set_items render ~count ~total;
    t.advance 0.02
  done;
  Task.set_items lint ~count:15 ~total;
  Task.succeed render;
  Task.fail lint;
  Display.redraw t.display;
  let rows = screen t in
  let msg = "in\n" ^ String.concat "\n" rows in
  let row l = List.find (contains l) rows in
  let render = row "render" and lint = row "lint" in
  let blanks row =
    let a = cell_of "render" row + 6 and b = cell_of "60/60" row in
    Array.fold_left
      (fun n u -> if Uchar.equal u (Uchar.of_int 0x20) then n + 1 else n)
      0
      (Array.sub (glyphs row) a (b - a))
  in
  Alcotest.(check bool) ("a full bar " ^ msg) true (full render);
  Alcotest.(check bool)
    ("no gap between the label and the count " ^ msg)
    true
    (blanks render < 5);
  Alcotest.check span
    ("the bar that failed in the same cells " ^ msg)
    (bar_span render) (bar_span lint);
  Alcotest.(check bool) ("a part of a bar " ^ msg) false (full lint);
  close t

(* Two trees side by side, as in examples/columns. At a fixed width nothing
   in the region moves sideways while counts grow, while a row with a wider
   count starts, and while a table's cells change. *)
let region_stands_still ~width ~bars =
  let t = terminal ~theme ~width ~height:16 () in
  let status = Hashtbl.create 2 in
  let table () =
    Table.view
      [
        Table.column (View.text "Build");
        Table.column ~min_width:7 (View.text "Status");
      ]
      (List.map
         (fun b ->
           [
             View.text b;
             View.text
               (Option.value (Hashtbl.find_opt status b) ~default:"waiting");
           ])
         [ "linux"; "macos" ])
  in
  let view d ~width ~height =
    match Task.children d with
    | [ a; b ] ->
        View.vcat
          [
            View.sized ~width (table ());
            View.hcat
              [
                View.sized ~width:((width - 2) / 2) ~height (Progress.tree a);
                View.sized ~width:((width - 2) / 2) ~height (Progress.tree b);
              ];
          ]
    | _ -> default d ~width ~height
  in
  Display.set_view t.display (fit t.root view);
  let linux = Task.start t.root "linux" and macos = Task.start t.root "macos" in
  Hashtbl.replace status "linux" "running";
  let fetch = Task.start macos "fetch" in
  let lfetch = Task.start linux "fetch" in
  let frames = ref [] in
  let frame () =
    Display.redraw t.display;
    let rows = screen t in
    let border = cells (List.hd rows) in
    let bar =
      List.find_map
        (fun r -> if contains "fetch" r then bar_end r else None)
        (List.rev rows)
    in
    frames := (border, bar, String.concat "\n" rows) :: !frames
  in
  for cur = 1 to 4 do
    Task.set_items fetch ~count:cur ~total:4;
    if cur <= 3 then Task.set_items lfetch ~count:cur ~total:4;
    if cur = 3 then begin
      Task.succeed lfetch;
      let compile = Task.start linux "compile" in
      Task.set_items compile ~count:12 ~total:12;
      Hashtbl.replace status "linux" "done"
    end;
    frame ()
  done;
  match List.rev !frames with
  | [] -> ()
  | (border, bar, rows) :: rest ->
      Alcotest.(check bool) ("a bar in\n" ^ rows) bars (bar <> None);
      List.iter
        (fun (b, r, rows) ->
          Alcotest.(check int) ("the table's edge in\n" ^ rows) border b;
          Alcotest.(check (option int)) ("the bar in\n" ^ rows) bar r)
        rest

let test_region_stands_still () =
  region_stands_still ~width:100 ~bars:true;
  region_stands_still ~width:80 ~bars:false

(* As in examples/progress: once a task counting bytes has its total, its
   first rate moves no bar sideways. *)
let test_rate_keeps_bars_still () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:0L ~total:2147483648L;
  Display.redraw t.display;
  let before = screen t in
  t.advance 0.2;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
  Display.redraw t.display;
  let after = screen t in
  let step_bar rows =
    List.find_map
      (fun r -> if contains "RUN make" r then bar_end r else None)
      rows
  in
  Alcotest.(check (option int))
    ("the bar of RUN make in\n" ^ String.concat "\n" before ^ "\nthen\n"
   ^ String.concat "\n" after)
    (step_bar before) (step_bar after);
  close t

(* The count and rate columns only widen, at most once for each new kind of
   progress, and never narrow. Rows of items are only as wide as their
   counts need. The first task of bytes widens the count column to the
   widest count of bytes and opens the rate column, once. Later tasks of
   bytes, larger totals and first rates move no bar. *)
let test_columns_widen_once () =
  let t = terminal ~theme ~width:80 ~height:10 () in
  let bar label rows =
    List.find_map (fun r -> if contains label r then bar_end r else None) rows
  in
  let step = Task.start t.root "RUN make" in
  Task.set_items step ~count:2 ~total:5;
  Display.redraw t.display;
  let items = screen t in
  let row = List.find (contains "RUN make") items in
  let count =
    let cells = Array.to_list (glyphs row) in
    let rec find i = function
      | [] -> None
      | u :: _
        when i > Option.get (bar_end row) && Uchar.equal u (Uchar.of_char '2')
        ->
          Some i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 cells
  in
  Alcotest.(check bool)
    ("the count close to the bar in\n" ^ row)
    true
    (Option.get count - Option.get (bar_end row) <= 8);
  let layer = Task.start step "sha256:abcd" in
  Task.set_bytes layer ~count:0L ~total:1073741824L;
  Display.redraw t.display;
  let first = screen t in
  t.advance 0.2;
  Task.set_bytes layer ~count:268435456L ~total:1073741824L;
  let other = Task.start step "sha256:ef01" in
  Task.set_bytes other ~count:0L ~total:549755813888L;
  t.advance 0.2;
  Task.set_bytes other ~count:274877906944L ~total:549755813888L;
  Task.set_bytes layer ~count:536870912L ~total:1073741824L;
  Display.redraw t.display;
  let later = screen t in
  Alcotest.(check (option int))
    ("the bar of RUN make in\n" ^ String.concat "\n" first ^ "\nthen\n"
   ^ String.concat "\n" later)
    (bar "RUN make" first) (bar "RUN make" later);
  Alcotest.(check (option int))
    ("the bars line up in\n" ^ String.concat "\n" later)
    (bar "RUN make" later) (bar "sha256:ef01" later);
  close t

let test_elapsed_rounded () =
  List.iter
    (fun (secs, shown) ->
      let now = ref 0. in
      let buffer = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buffer in
      Theme.set ppf theme;
      let d, display = show ~progress:`Plain ~now:(fun () -> !now) ppf in
      let t = Task.start d "t" in
      now := secs;
      Task.succeed t;
      Display.close display;
      Format.pp_print_flush ppf ();
      let out = Buffer.contents buffer in
      Alcotest.(check bool)
        (Fmt.str "%g s reads %s in %S" secs shown out)
        true
        (contains ("t  " ^ shown) out))
    [
      (0.96, "1.0s");
      (0.94, "0.9s");
      (59.97, "1m00s");
      (119.6, "2m00s");
      (3599.6, "1h00m");
    ]

let suite =
  ( "progress_row",
    [
      Alcotest.test_case "alignment" `Quick test_alignment;
      Alcotest.test_case "label truncated" `Quick test_label_truncated;
      Alcotest.test_case "bytes fit their column" `Quick test_bytes_width;
      Alcotest.test_case "the label column of drawn labels" `Quick
        test_label_column_drawn;
      Alcotest.test_case "the label gives way after the bar" `Quick
        test_label_before_bar;
      Alcotest.test_case "the label gives way after the bar, side by side"
        `Quick test_label_before_bar_hcat;
      Alcotest.test_case "the last frame keeps labels readable" `Quick
        test_last_frame_label_readable;
      Alcotest.test_case "a label is not cut for an empty count" `Quick
        test_label_not_cut_for_an_empty_count;
      Alcotest.test_case "the count and the time give way after the label"
        `Quick test_count_and_time_after_label;
      Alcotest.test_case "the bars line up" `Quick test_bars_line_up;
      Alcotest.test_case "rows pack from the left" `Quick test_rows_pack_left;
      Alcotest.test_case "the bar shrinks before anything gives way" `Quick
        test_bar_shrinks_first;
      Alcotest.test_case "a short count of bytes keeps to three digits" `Quick
        test_short_bytes;
      Alcotest.test_case "a task that ends keeps its columns" `Quick
        test_ended_keeps_columns;
      Alcotest.test_case "the count and the time keep their columns" `Quick
        test_count_and_time_padded;
      Alcotest.test_case "a fixed label column moves nothing" `Quick
        test_replaced_label_widens_nothing;
      Alcotest.test_case "the label is cut before the count and the time" `Quick
        test_label_cut_before_count;
      Alcotest.test_case "the label column never narrows" `Quick
        test_running_label_kept;
      Alcotest.test_case "a printed row keeps its width" `Quick
        test_printed_width_kept;
      Alcotest.test_case "steps share their columns" `Quick
        test_steps_share_columns;
      Alcotest.test_case "the tasks that wait fit the region" `Quick
        test_held_tasks_fit;
      Alcotest.test_case "a task that ended draws its bar" `Quick test_ended_bar;
      Alcotest.test_case "an ended bar takes the colour of its end" `Quick
        test_ended_bar_colour;
      Alcotest.test_case "the columns widen once for each kind of progress"
        `Quick test_columns_widen_once;
      Alcotest.test_case "a first rate moves no bar" `Quick
        test_rate_keeps_bars_still;
      Alcotest.test_case "elapsed times are rounded" `Quick test_elapsed_rounded;
      Alcotest.test_case "the region stands still" `Quick
        test_region_stands_still;
    ] )
