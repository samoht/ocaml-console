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
let sink = ref 0

let widths () =
  run "width ascii 80" ~n:100_000 (fun () ->
      sink := Span.width (Span.text ascii));
  run "width cjk 40" ~n:100_000 (fun () -> sink := Span.width (Span.text cjk));
  run "width emoji 20" ~n:10_000 (fun () ->
      sink := Span.width (Span.text emoji))

let fitting () =
  let span = Span.text prose in
  run "truncate prose to 40" ~n:10_000 (fun () ->
      sink := Span.width (Span.truncate 40 span));
  run "wrap prose to 30" ~n:10_000 (fun () ->
      sink := List.length (Span.wrap 30 span))

let null = Format.make_formatter (fun _ _ _ -> ()) ignore

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
  run "table 1k rows" ~n:10 (fun () -> Table.pp null (Table.v columns rows))

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
  idle ();
  run "display plain 10k tasks 100k logs" ~n:1 (display ~live:false);
  run "display live 10k tasks 100k logs" ~n:1 (display ~live:true)
