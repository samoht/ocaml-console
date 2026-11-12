(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The cost of redrawing, on a virtual clock, with 1 to 1000 tasks in view.

   1. A frame: its allocated words (deterministic) and its processor time.
      There are two cases: tasks still (only the time moved), and every
      task advanced since the last frame.
   2. Coalescing: [rate] updates per simulated second, spread over the
      tasks. It prints the frames drawn per simulated second, which must
      not depend on the rate.

   With [--check], it fails if a simulated second draws more than 11
   frames. A frame's words are printed next to the recorded count but not
   checked: most come from Format and Fmt, and change with their releases.
   Usage: render.exe [--check] *)

open Console

let check = Array.exists (String.equal "--check") Sys.argv
let visible = if check then [ 1; 10; 100 ] else [ 1; 10; 100; 1000 ]

let rates =
  if check then [ 10; 1_000; 10_000 ] else [ 10; 1_000; 100_000; 1_000_000 ]

(* The number of frames a case draws. [--check] draws fewer, because it
   needs only their words. *)
let frames_of tasks = max 10 ((if check then 2_000 else 20_000) / tasks)

(* The words a frame may allocate, by tasks in view and whether they moved.
   [ratchet] is what the code allocates now, the larger of the flambda and
   non-flambda counts. [target] is a thousand words a row. *)
let ratchet tasks busy =
  match (tasks, busy) with
  | 1, false -> 4_036.
  | 1, true -> 5_018.
  | 10, false -> 28_177.
  | 10, true -> 50_910.
  | 100, false -> 269_861.
  | 100, true -> 604_580.
  | _ -> infinity

let target tasks = 1000. *. float (tasks + 1)
let max_fps = 11.

(* A terminal size that fits the rows of [n] tasks and the summary below
   them. *)
let size n () = (100, n + 8)

let setup ~now tasks =
  let root = Task.root ~label:"bench" ~now () in
  let sink = Measure.sink () in
  let d = Progress.show ~progress:`Tty ~size:(size tasks) sink.ppf root in
  let ts =
    Array.init tasks (fun i ->
        let r = Fmt.kstr (Task.start root) "task %04d" i in
        Task.set_bytes r ~count:0L ~total:Int64.max_int;
        r)
  in
  (d, sink, ts)

let frames () =
  Fmt.pr "a frame, virtual clock (100 columns, every row in view)@.";
  Fmt.pr "%6s %-8s %10s %12s %10s@." "tasks" "rows" "cpu us" "words" "words/row";
  List.iter
    (fun tasks ->
      List.iter
        (fun busy ->
          let t = ref 0. in
          let d, _, ts = setup ~now:(fun () -> !t) tasks in
          let n = frames_of tasks in
          let advance () = Array.iter (fun r -> Task.advance ~by:65536 r) ts in
          let frame () =
            t := !t +. 0.1;
            if busy then advance ();
            Display.redraw d
          in
          for _ = 1 to 5 do
            frame ()
          done;
          (* Measure the cost of the advances alone, to subtract it. *)
          let adv_w, adv_c =
            if not busy then (0., 0.)
            else begin
              let w0 = Measure.words () and c0 = Sys.time () in
              for _ = 1 to n do
                advance ()
              done;
              ( (Measure.words () -. w0) /. float n,
                (Sys.time () -. c0) /. float n )
            end
          in
          let w0 = Measure.words () and c0 = Sys.time () in
          for _ = 1 to n do
            frame ()
          done;
          let c = ((Sys.time () -. c0) /. float n) -. adv_c in
          let w = ((Measure.words () -. w0) /. float n) -. adv_w in
          Display.close d;
          Fmt.pr "%6d %-8s %10.1f %12.0f %10.0f  %s@." tasks
            (if busy then "advanced" else "still")
            (c *. 1e6) w
            (w /. float tasks)
            (Measure.verdict ~gated:false ~ratchet:(ratchet tasks busy)
               ~target:(target tasks) w))
        [ false; true ])
    visible

(* For each number of tasks and each rate, run [rate * secs] updates on a
   virtual clock and print the frames drawn per simulated second. *)
let coalescing () =
  Fmt.pr "@.coalescing, virtual clock: frames drawn per simulated second@.";
  Fmt.pr "%6s" "tasks";
  List.iter (fun r -> Fmt.pr " %9d/s" r) rates;
  Fmt.pr "@.";
  List.iter
    (fun tasks ->
      Fmt.pr "%6d" tasks;
      List.iter
        (fun rate ->
          let secs = 2 in
          let t = ref 0. in
          let d, sink, ts = setup ~now:(fun () -> !t) tasks in
          let dt = 1. /. float rate and f0 = !(sink.frames) in
          for i = 0 to (rate * secs) - 1 do
            t := !t +. dt;
            Task.advance ~by:65536 ts.(i mod tasks)
          done;
          let fps = float (!(sink.frames) - f0) /. float secs in
          Display.close d;
          if fps > max_fps then incr Measure.failures;
          Fmt.pr " %11.1f" fps)
        rates;
      Fmt.pr "@.")
    [ 1; 100 ]

let () =
  Unix.putenv "COLUMNS" "100";
  frames ();
  coalescing ();
  if check then Measure.exit_on_failures ()
