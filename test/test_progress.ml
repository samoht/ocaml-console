(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains affix s = Astring.String.is_infix ~affix s

let test_names () =
  Alcotest.(check (list string))
    "names"
    [ "auto"; "tty"; "plain"; "quiet"; "json" ]
    (List.map fst Progress.all);
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool)
        ("of_string " ^ name) true
        (match Progress.of_string name with
        | Ok p' -> p' = p
        | Error _ -> false))
    Progress.all;
  match Progress.of_string "silent" with
  | Ok _ -> Alcotest.fail "unknown name accepted"
  | Error (`Msg m) ->
      Alcotest.(check bool) "names the choices" true (contains "quiet" m)

(* What a display given [progress] prints for a task that succeeds, with a
   line of output, and one that fails, with another, under a header. *)
let shown ?size progress =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let d =
    Display.v ?size ~progress ~header:"Building" ~now:(fun () -> 0.) ppf
  in
  Display.with_task d "fine" (fun t -> Display.log t "quiet line");
  (try
     Display.with_task d "broken" (fun t ->
         Display.log t "loud line";
         failwith "broken")
   with Failure _ -> ());
  Display.close d;
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let lines s = List.filter (( <> ) "") (String.split_on_char '\n' s)

(* [`Quiet] prints the failure, its line and its output, and nothing else,
   also when [size] says there is a terminal. *)
let test_quiet () =
  List.iter
    (fun size ->
      let out = shown ?size `Quiet in
      Alcotest.(check bool) "the failure" true (contains "broken" out);
      Alcotest.(check bool) "its output" true (contains "loud line" out);
      Alcotest.(check bool) "no success" false (contains "fine" out);
      Alcotest.(check bool)
        "no output of a success" false
        (contains "quiet line" out);
      Alcotest.(check bool) "no header" false (contains "Building" out);
      Alcotest.(check bool) "no escape" false (String.contains out '\027'))
    [ None; Some (fun () -> (80, 24)) ]

(* [`Json] prints one event a line, and nothing else. *)
let test_json () =
  List.iter
    (fun size ->
      let out = shown ?size `Json in
      List.iter
        (fun line ->
          Alcotest.(check bool) line true (contains "{\"time\":" line))
        (lines out);
      Alcotest.(check bool)
        "an output event" true
        (contains "\"text\":\"quiet line\"" out);
      Alcotest.(check bool)
        "an end event" true
        (contains "\"status\":\"failed\"" out))
    [ None; Some (fun () -> (80, 24)) ]

(* [`Plain] prints the lines of a pipe on a terminal too, and [`Auto] as
   [size] says. *)
let test_plain () =
  let plain = shown `Plain in
  Alcotest.(check string)
    "on a terminal" plain
    (shown ~size:(fun () -> (80, 24)) `Plain);
  Alcotest.(check string) "auto on a pipe" plain (shown `Auto);
  Alcotest.(check bool)
    "plain has no escape" false
    (String.contains plain '\027');
  Alcotest.(check bool) "a line for the success" true (contains "fine" plain)

(* [`Tty] redraws the rows, on 80 by 24 without [size]. *)
let test_tty () =
  let hide = "\027[?25l" in
  Alcotest.(check bool) "tty hides the cursor" true (contains hide (shown `Tty));
  Alcotest.(check bool)
    "auto on a terminal too" true
    (contains hide (shown ~size:(fun () -> (80, 24)) `Auto))

let suite =
  ( "progress",
    [
      Alcotest.test_case "names" `Quick test_names;
      Alcotest.test_case "quiet" `Quick test_quiet;
      Alcotest.test_case "json" `Quick test_json;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "tty" `Quick test_tty;
    ] )
