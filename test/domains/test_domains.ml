(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let workers = 4
let in_workers f = List.init workers (fun d -> Domain.spawn (fun () -> f d))
let join_all ds = List.map Domain.join ds
let now () = 0.

let items t =
  match Task.progress t with
  | `Items (count, total) -> (count, total)
  | _ -> Alcotest.fail "not a count of items"

let test_advance_counts_every_step () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  let steps = 50_000 in
  Task.set_items t ~count:0 ~total:(workers * steps);
  ignore
    (join_all
       (in_workers (fun _ ->
            for _ = 1 to steps do
              Task.advance t
            done)));
  Alcotest.(check (pair int int))
    "every step"
    (workers * steps, workers * steps)
    (items t)

let test_advance_after_set_items () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  ignore
    (Domain.join
       (Domain.spawn (fun () ->
            Task.set_items t ~count:3 ~total:10;
            Task.advance ~by:2 t;
            Task.set_items t ~count:7 ~total:10;
            Task.advance t)));
  Alcotest.(check (pair int int)) "in the order given" (8, 10) (items t)

let test_changes_stay_on_the_owner () =
  let root = Task.root ~now () in
  let seen = ref [] in
  Task.on_event root (fun _ -> seen := (Domain.self () :> int) :: !seen);
  ignore
    (join_all
       (in_workers (fun d ->
            Task.run root (Fmt.str "job %d" d) (fun t ->
                Task.set_items t ~count:1 ~total:2;
                Task.log t "line";
                Task.set_label t "renamed"))));
  ignore (Task.state root : Task.state);
  let owner = (Domain.self () :> int) in
  Alcotest.(check bool) "events came" true (!seen <> []);
  List.iter (Alcotest.(check int) "the owner" owner) !seen

let test_run_from_a_worker () =
  let root = Task.root ~now () in
  let results =
    join_all
      (in_workers (fun d ->
           Task.run root (Fmt.str "job %d" d) (fun t ->
               Task.set_items t ~count:1 ~total:1;
               d)))
  in
  Alcotest.(check (list int)) "each result" [ 0; 1; 2; 3 ] results;
  let children = Task.children root in
  Alcotest.(check int) "each task" workers (List.length children);
  List.iter
    (fun c ->
      Alcotest.(check bool) "succeeded" true (Task.state c = `Succeeded))
    children

let test_failure_from_a_worker () =
  let root = Task.root ~now () in
  let t = ref None in
  ignore
    (Domain.join
       (Domain.spawn (fun () ->
            try
              Task.run root "job" (fun c ->
                  t := Some c;
                  failwith "boom")
            with Failure _ -> ())));
  let t = Option.get !t in
  Alcotest.(check bool) "failed" true (Task.state t = `Failed);
  Alcotest.(check (option string))
    "says why" (Some {|job: Failure("boom")|}) (Task.message t)

let test_a_key_names_one_task () =
  let root = Task.root ~now () in
  let tasks =
    join_all (in_workers (fun _ -> Task.start ~key:"same" root "shared"))
  in
  let first = List.hd tasks in
  List.iter (fun t -> Alcotest.(check bool) "same" true (t == first)) tasks;
  Alcotest.(check int) "one row" 1 (List.length (Task.children root))

let test_lines_keep_their_order () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  let lines = 500 in
  ignore
    (join_all
       (in_workers (fun d ->
            for i = 1 to lines do
              Task.log t (Fmt.str "%d %d" d i)
            done)));
  let out = Task.output t in
  Alcotest.(check int) "all of them" (workers * lines) (List.length out);
  for d = 0 to workers - 1 do
    let mine =
      List.filter (String.starts_with ~prefix:(string_of_int d ^ " ")) out
    in
    Alcotest.(check (list string))
      "in order"
      (List.init lines (fun i -> Fmt.str "%d %d" d (i + 1)))
      mine
  done

let flood t =
  let padding = String.make 4096 'x' in
  for i = 1 to 4096 do
    Task.log t (Fmt.str "%d %s" i padding)
  done

let number line = int_of_string (List.hd (String.split_on_char ' ' line))

(* If the owner never reads, the worker pays only for the lines held back. The
   worker goes on. The owner shows the newest lines, after a line that says how
   many older lines it lost. *)
let test_a_flood_keeps_the_newest_lines () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  let said = ref [] in
  Task.on_event root (fun e ->
      match Task.Event.text e with Some s -> said := s :: !said | None -> ());
  ignore (Domain.join (Domain.spawn (fun () -> flood t)));
  ignore (Task.state t : Task.state);
  match List.rev !said with
  | [] -> Alcotest.fail "nothing shown"
  | notice :: kept ->
      let lost = 4096 - List.length kept in
      Alcotest.(check bool) "some are lost" true (lost > 0);
      Alcotest.(check string)
        "says how many"
        (Fmt.str "[console: %d older lines from other domains dropped]" lost)
        notice;
      Alcotest.(check (list int))
        "the newest, in order"
        (List.init (List.length kept) (fun i -> lost + i + 1))
        (List.map number kept)

(* Losing lines loses nothing else, and everything else happens in the order it
   was requested. *)
let test_a_flood_loses_only_lines () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  let child =
    Domain.join
      (Domain.spawn (fun () ->
           flood t;
           let child = Task.start t "child" in
           Task.log child "on the child";
           Task.log t "before the end";
           Task.fail t;
           Task.log t "after the end";
           child))
  in
  Alcotest.(check bool) "ended" true (Task.state t = `Failed);
  Alcotest.(check bool)
    "the line before its end" true
    (List.mem "before the end" (Task.output t));
  Alcotest.(check bool)
    "no line after it" false
    (List.mem "after the end" (Task.output t));
  Alcotest.(check (list string))
    "the child" [ "on the child" ] (Task.output child)

let test_steps_after_the_end_are_ignored () =
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  Task.set_items t ~count:1 ~total:2;
  Task.succeed t;
  ignore
    (Domain.join
       (Domain.spawn (fun () ->
            Task.advance t;
            Task.fail t)));
  Alcotest.(check bool) "first end counts" true (Task.state t = `Succeeded);
  Alcotest.(check (pair int int)) "no later step" (1, 2) (items t)

let with_buffer f =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  f b ppf

let test_display_from_a_worker () =
  with_buffer @@ fun b ppf ->
  let d = Display.v ~now ppf (View.text "region") in
  ignore
    (Domain.join
       (Domain.spawn (fun () ->
            Display.print d (View.text "printed");
            let v =
              Display.with_output d (fun ppf ->
                  Format.pp_print_string ppf "written\n";
                  42)
            in
            Alcotest.(check int) "its value" 42 v;
            Display.set_view d (View.text "after"))));
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents b in
  let has s = Astring.String.is_infix ~affix:s out in
  Alcotest.(check bool) "printed" true (has "printed");
  Alcotest.(check bool) "written" true (has "written");
  Alcotest.(check bool) "the new view last" true (has "after")

let test_close_from_a_worker () =
  with_buffer @@ fun _ ppf ->
  let d = Display.v ~now ppf (View.text "region") in
  ignore (Domain.join (Domain.spawn (fun () -> Display.close d)));
  Display.redraw d;
  Display.close d

(* What a worker sends reaches a display only between frames. A view that reads
   a task does not start another frame inside the frame it draws. *)
let test_a_view_reads_between_frames () =
  with_buffer @@ fun _ ppf ->
  let root = Task.root ~now () in
  let t = Task.start root "t" in
  Task.set_items t ~count:0 ~total:100;
  let depth = ref 0 and deepest = ref 0 in
  let view =
    View.fit (fun _ ->
        incr depth;
        deepest := max !deepest !depth;
        ignore (Task.progress t : Task.progress);
        decr depth;
        View.text "row")
  in
  let d =
    Progress.show ~progress:`Tty ~view ~size:(fun () -> (40, 10)) ppf root
  in
  ignore
    (Domain.join
       (Domain.spawn (fun () ->
            for _ = 1 to 10 do
              Task.advance t;
              Task.log t "line"
            done)));
  Display.redraw d;
  Display.close d;
  Alcotest.(check int) "one frame at a time" 1 !deepest

let suite =
  ( "domains",
    [
      Alcotest.test_case "advance counts every step" `Quick
        test_advance_counts_every_step;
      Alcotest.test_case "advance after set_items" `Quick
        test_advance_after_set_items;
      Alcotest.test_case "changes stay on the owner" `Quick
        test_changes_stay_on_the_owner;
      Alcotest.test_case "run from a worker" `Quick test_run_from_a_worker;
      Alcotest.test_case "failure from a worker" `Quick
        test_failure_from_a_worker;
      Alcotest.test_case "a key names one task" `Quick test_a_key_names_one_task;
      Alcotest.test_case "lines keep their order" `Quick
        test_lines_keep_their_order;
      Alcotest.test_case "a flood keeps the newest lines" `Quick
        test_a_flood_keeps_the_newest_lines;
      Alcotest.test_case "a flood loses only lines" `Quick
        test_a_flood_loses_only_lines;
      Alcotest.test_case "steps after the end are ignored" `Quick
        test_steps_after_the_end_are_ignored;
      Alcotest.test_case "display from a worker" `Quick
        test_display_from_a_worker;
      Alcotest.test_case "close from a worker" `Quick test_close_from_a_worker;
      Alcotest.test_case "a view reads between frames" `Quick
        test_a_view_reads_between_frames;
    ] )
