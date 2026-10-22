(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* [run name ~n f] runs [f] [n] times and prints the processor time and the
   words allocated per run. Processor time, not wall-clock time, so that a
   busy machine does not count against the code. *)
let run name ~n f =
  f ();
  Gc.full_major ();
  let words () =
    let minor, promoted, major = Gc.counters () in
    minor +. major -. promoted
  in
  let w0 = words () and t0 = Sys.time () in
  for _ = 1 to n do
    f ()
  done;
  let t1 = Sys.time () and w1 = words () in
  Fmt.pr "%-34s %12.0f ns/op %14.0f words/op@." name
    ((t1 -. t0) *. 1e9 /. float n)
    ((w1 -. w0) /. float n)

let ascii = String.make 80 'a'
let cjk = String.concat "" (List.init 40 (fun _ -> "界"))
let emoji = String.concat "" (List.init 20 (fun _ -> "👨‍👩‍👧"))
let prose = String.concat " " (List.init 40 (fun i -> Fmt.str "word%d" i))

(* Text of every kind of cluster: accented Latin, a combining mark, CJK, a
   Devanagari consonant with its vowel sign, an emoji, a skin tone, a flag, a
   heart with VS16 and a ZWJ family; 80 cells. *)
let mixed =
  String.concat ""
    (List.init 4 (fun _ ->
         "caf\u{00E9} e\u{0301} \u{754C}\u{754C} \u{0915}\u{093F} \u{1F389} \
          \u{1F44D}\u{1F3FD} \u{1F1EB}\u{1F1F7} \u{2764}\u{FE0F} \
          \u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}"))

let long_ascii = String.concat " " (List.init 2000 (fun _ -> "word"))
let long_mixed = String.concat " " (List.init 125 (fun _ -> mixed))
let sink = ref 0

let widths () =
  run "width ascii 80" ~n:100_000 (fun () ->
      sink := Span.width (Span.text ascii));
  run "width cjk 40" ~n:100_000 (fun () -> sink := Span.width (Span.text cjk));
  run "width emoji 20" ~n:10_000 (fun () ->
      sink := Span.width (Span.text emoji));
  run "width mixed 80" ~n:10_000 (fun () ->
      sink := Span.width (Span.text mixed));
  run "width ascii 10k" ~n:1_000 (fun () ->
      sink := Span.width (Span.text long_ascii));
  run "width mixed 10k" ~n:100 (fun () ->
      sink := Span.width (Span.text long_mixed))

let fitting () =
  let span = Span.text prose in
  run "truncate prose to 40" ~n:10_000 (fun () ->
      sink := Span.width (Span.truncate 40 span));
  run "wrap prose to 30" ~n:10_000 (fun () ->
      sink := List.length (Span.wrap 30 span));
  let span = Span.text mixed in
  run "truncate mixed to 40" ~n:10_000 (fun () ->
      sink := Span.width (Span.truncate 40 span));
  run "wrap mixed to 30" ~n:10_000 (fun () ->
      sink := List.length (Span.wrap 30 span));
  let span = Span.text long_mixed in
  run "wrap mixed 10k to 80" ~n:100 (fun () ->
      sink := List.length (Span.wrap 80 span))

let null = Format.make_formatter (fun _ _ _ -> ()) ignore

let utf_8 =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) ignore in
  Fmt.set_utf_8 ppf true;
  Format.pp_set_margin ppf 120;
  ppf

(* Bars and rules fill their cells with a glyph, cluster by cluster. *)
let fills () =
  run "bar 80 cells" ~n:10_000 (fun () -> Bar.pp utf_8 (Bar.v ~width:80 0.5));
  let rule = Rule.v ~glyph:"\u{2550}" ~label:(Span.text mixed) () in
  run "rule 120 cells, mixed label" ~n:10_000 (fun () -> Rule.pp utf_8 rule)

(* The line editor measures the line and steps over its clusters as it
   echoes. *)
let line_editor () =
  run "line editor mixed 80" ~n:1_000 (fun () ->
      let t = Console_input.v () in
      let echo, _ = Console_input.feed t mixed in
      let back, _ = Console_input.feed t "\x1b[D\x1b[D\x1b[D\x7f\r" in
      sink := String.length echo + String.length back)

let table () =
  let rows =
    List.init 1000 (fun i ->
        [ Fmt.kstr Span.text "row %d" i; Span.text prose; Span.text "42" ])
  in
  let columns =
    Table.
      [
        column (Span.text "Name");
        column (Span.text "Text");
        column ~align:`Right (Span.text "N");
      ]
  in
  Format.pp_set_margin null 120;
  run "table 1k rows" ~n:10 (fun () -> Table.pp null (Table.v columns rows));
  let rows =
    List.init 1000 (fun i ->
        [ Fmt.kstr Span.text "row %d" i; Span.text mixed; Span.text "42" ])
  in
  run "table 1k mixed rows" ~n:10 (fun () ->
      Table.pp utf_8 (Table.v columns rows))

let tree () =
  let leaf i = Tree.v (Fmt.kstr Span.text "leaf %d" i) [] in
  let tree =
    Tree.v (Span.text "root")
      (List.init 10 (fun i ->
           Tree.v (Fmt.kstr Span.text "dir %d" i) (List.init 100 leaf)))
  in
  run "tree 1k nodes" ~n:10 (fun () -> Tree.pp null tree)

let clock () =
  let t = ref 0. in
  ((fun () -> !t), fun () -> t := !t +. 0.001)

let display ~live () =
  let now, tick = clock () in
  let size = if live then Some (fun () -> (80, 24)) else None in
  let d = Display.v ?size ~now ~header:"bench" null in
  for i = 1 to 10_000 do
    let t = Fmt.kstr (Display.task d) "task %d" i in
    for j = 1 to 10 do
      tick ();
      Display.set_count t ~cur:j ~total:10;
      Display.log t "a line of output from the task"
    done;
    Display.succeed t
  done;
  Display.close d

(* Ten tasks run while the clock does not move: every redraw is the frame
   already on screen. *)
let idle () =
  let now () = 0. in
  let d = Display.v ~size:(fun () -> (80, 24)) ~now ~header:"bench" null in
  let tasks = List.init 10 (fun i -> Fmt.kstr (Display.task d) "task %d" i) in
  run "redraw with nothing changed" ~n:10_000 (fun () -> Display.redraw d);
  List.iter Display.succeed tasks;
  Display.close d

let () =
  widths ();
  fitting ();
  table ();
  tree ();
  fills ();
  line_editor ();
  idle ();
  run "display plain 10k tasks 100k logs" ~n:1 (display ~live:false);
  run "display live 10k tasks 100k logs" ~n:1 (display ~live:true)
