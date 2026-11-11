(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cast = Console__.Cast

let decode t s = Jsont_bytesrw.decode_string t s

let refused msg t s =
  Alcotest.(check bool) msg true (Result.is_error (decode t s))

(* The example from the asciicast v2 documentation. The members that Cast
   does not use are optional, and the reader ignores them. *)
let test_header_example () =
  match
    decode Cast.header
      {|{"version": 2, "width": 80, "height": 24, "timestamp": 1504467315,
         "title": "Demo", "env": {"TERM": "xterm-256color", "SHELL": "/bin/zsh"}}|}
  with
  | Error e -> Alcotest.fail e
  | Ok { width; height; term } ->
      Alcotest.(check (pair int int)) "size" (80, 24) (width, height);
      Alcotest.(check (option string)) "TERM" (Some "xterm-256color") term

(* The header requires version, width and height. *)
let test_header_required () =
  refused "another version" Cast.header {|{"version":1,"width":80,"height":24}|};
  refused "no version" Cast.header {|{"width":80,"height":24}|};
  refused "no width" Cast.header {|{"version":2,"height":24}|};
  refused "no height" Cast.header {|{"version":2,"width":80}|};
  match decode Cast.header {|{"version":2,"width":80,"height":24}|} with
  | Ok { term = None; _ } -> ()
  | Ok _ -> Alcotest.fail "a TERM from nowhere"
  | Error e -> Alcotest.fail e

let test_event_example () =
  match decode Cast.event {|[1.001376, "o", "Hello world"]|} with
  | Error e -> Alcotest.fail e
  | Ok { time; data } ->
      Alcotest.(check (float 0.)) "time" 1.001376 time;
      Alcotest.(check string) "data" "Hello world" data

(* [Cast.event] reads output events only. It refuses an "i" (input) event,
   an event without data, and an event whose time is not a number. *)
let test_event_refused () =
  refused "input" Cast.event {|[1.0, "i", "x"]|};
  refused "no data" Cast.event {|[1.0, "o"]|};
  refused "a time that is not a number" Cast.event {|["1.0", "o", "x"]|}

(* Newline-delimited JSON: each line holds one value, whatever the data
   contains. *)
let test_one_line () =
  let line = Cast.to_line Cast.event { time = 0.; data = "a\nb\r\n\027[0m" } in
  Alcotest.(check bool) "ended" true (String.ends_with ~suffix:"\n" line);
  Alcotest.(check int)
    "one line" 1
    (List.length (String.split_on_char '\n' (String.trim line)));
  Alcotest.(check string)
    "reads back" "a\nb\r\n\027[0m" (Cast_harness.event line).data

(* asciicast v2, "o": data is valid UTF-8. Each maximal subpart of an
   ill-formed sequence becomes one U+FFFD (Unicode section 3.9). *)
let test_maximal_subparts () =
  List.iter
    (fun (table, bytes, converted) ->
      let line = Cast.to_line Cast.event { time = 0.; data = bytes } in
      Alcotest.(check string) table converted (Cast_harness.event line).data)
    Text_harness.maximal_subparts

let test_header_term () =
  let line term = Cast.to_line Cast.header { width = 80; height = 24; term } in
  Alcotest.check Cast_harness.json "no TERM, no env"
    (Cast_harness.parse {|{"version":2,"width":80,"height":24}|})
    (Cast_harness.parse (line None));
  Alcotest.check Cast_harness.json "a TERM"
    (Cast_harness.parse
       {|{"version":2,"width":80,"height":24,"env":{"TERM":"xterm"}}|})
    (Cast_harness.parse (line (Some "xterm")))

let suite =
  ( "cast",
    [
      Alcotest.test_case "the header of the format's example" `Quick
        test_header_example;
      Alcotest.test_case "what a header requires" `Quick test_header_required;
      Alcotest.test_case "the event of the format's example" `Quick
        test_event_example;
      Alcotest.test_case "what an event refuses" `Quick test_event_refused;
      Alcotest.test_case "one line" `Quick test_one_line;
      Alcotest.test_case "maximal subparts" `Quick test_maximal_subparts;
      Alcotest.test_case "the TERM of a header" `Quick test_header_term;
    ] )
