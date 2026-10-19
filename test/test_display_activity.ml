(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_activity_snapshot () =
  let (before, after, final), _ =
    capture (fun display advance ->
        let parent = Display.task display "download" in
        let child = Display.task ~parent ~log_lines:2 display "layer" in
        Display.set_bytes child ~cur:4L ~total:10L;
        Display.log child "one";
        Display.log ~level:`Warning child "two";
        Display.log child "three";
        advance 2.;
        let before = Display.Activity.items (Display.activity display) in
        Display.succeed child;
        let after = Display.Activity.items (Display.activity display) in
        Display.succeed parent;
        (before, after, Display.activity display))
  in
  Alcotest.(check int) "two running" 2 (List.length before);
  let child = List.nth before 1 in
  Alcotest.(check string) "label" "layer" (Display.Activity.label child);
  let logs = Display.Activity.logs child in
  Alcotest.(check (list string))
    "bounded tail" [ "two"; "three" ]
    (List.map Display.Activity.log_message logs);
  Alcotest.(check bool)
    "severity retained" true
    (List.map Display.Activity.log_level logs = [ `Warning; `Info ]);
  Alcotest.(check bool)
    "bytes" true
    (Display.Activity.equal_progress
       (Display.Activity.progress child)
       (`Bytes (4L, 10L)));
  Alcotest.(check int) "finished child absent" 1 (List.length after);
  Alcotest.(check int)
    "nothing active after completion" 0
    (List.length (Display.Activity.items final))

let test_keyed_task () =
  let events, _ =
    capture (fun display _ ->
        let first = Display.task ~id:"compile:foo" display "waiting" in
        let second = Display.task ~id:"compile:foo" display "ignored" in
        Alcotest.(check bool) "stable handle" true (first == second);
        Display.set_label second "compile foo.ml";
        Display.set_count first ~cur:8 ~total:10;
        Display.set_count second ~cur:3 ~total:4;
        let item =
          List.hd (Display.Activity.items (Display.activity display))
        in
        Alcotest.(check bool)
          "monotonic progress" true
          (Display.Activity.equal_progress
             (Display.Activity.progress item)
             (`Count (8, 10)));
        Alcotest.(check (option string))
          "semantic key" (Some "compile:foo")
          (Display.Activity.key item);
        Display.succeed first;
        Display.history display)
  in
  Alcotest.(check bool)
    "one lifecycle" true
    (List.map Display.Event.kind events = [ `Started; `Finished `Succeeded ]);
  Alcotest.(check (list (option string)))
    "key retained"
    [ Some "compile:foo"; Some "compile:foo" ]
    (List.map Display.Event.key events)

let test_progress_estimates_expand () =
  let item, _ =
    capture (fun display _ ->
        Display.with_task ~total:8 display "split" (fun task ->
            Display.set_count task ~cur:22 ~total:8;
            Display.set_count task ~cur:23 ~total:874;
            Display.advance ~label:"writing objects" task;
            List.hd (Display.Activity.items (Display.activity display))))
  in
  Alcotest.(check string)
    "label updated atomically" "writing objects"
    (Display.Activity.label item);
  Alcotest.(check bool)
    "estimate grew and incremented" true
    (Display.Activity.equal_progress
       (Display.Activity.progress item)
       (`Count (24, 874)));
  let bytes_item, _ =
    capture (fun display _ ->
        Display.with_task display "download" (fun task ->
            Display.set_bytes task ~cur:22L ~total:8L;
            List.hd (Display.Activity.items (Display.activity display))))
  in
  Alcotest.(check bool)
    "byte estimate grew" true
    (Display.Activity.equal_progress
       (Display.Activity.progress bytes_item)
       (`Bytes (22L, 22L)))

(* An event stream carries one id across every scope its emitter passes
   through, so the same id arrives under a second parent as a matter of course.
   Offering one is a statement about where the work is, never a request to move
   a row that already exists: the first parent wins and nothing raises, because
   a display that can raise here can abort the operation it is drawing. *)
let test_key_keeps_its_first_parent () =
  let items =
    fst
    @@ capture (fun display _ ->
        let first = Display.task display "first" in
        let second = Display.task display "second" in
        let child = Display.task ~id:"child" ~parent:first display "child" in
        let again = Display.task ~id:"child" ~parent:second display "child" in
        Alcotest.(check bool)
          "the second offer returns the same task" true (again == child);
        List.map
          (fun item -> (Display.Activity.label item, item))
          (Display.Activity.items (Display.activity display)))
  in
  let item label =
    match List.assoc_opt label items with
    | Some item -> item
    | None -> Alcotest.failf "no %s row" label
  in
  Alcotest.(check (option int))
    "the child stays under the parent it started in"
    (Some (Display.Activity.id (item "first")))
    (Display.Activity.parent (item "child"));
  Alcotest.(check int) "no second row was created" 3 (List.length items)

let suite =
  ( "display_activity",
    [
      Alcotest.test_case "activity snapshot" `Quick test_activity_snapshot;
      Alcotest.test_case "keyed task" `Quick test_keyed_task;
      Alcotest.test_case "progress estimates expand" `Quick
        test_progress_estimates_expand;
      Alcotest.test_case "key keeps its first parent" `Quick
        test_key_keeps_its_first_parent;
    ] )
