(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One record of the byte stream BSD script(1) -r writes: the payload length,
   the wall clock, the kind, then the payload, little-endian throughout. The
   seconds and microseconds are given apart so the fixture states exact times
   rather than rounding a float into a field. *)
let record ~kind ~sec ~usec payload =
  let buffer = Buffer.create (24 + String.length payload) in
  let add64 v =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 (Int64.of_int v);
    Buffer.add_bytes buffer b
  in
  let add32 v =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int v);
    Buffer.add_bytes buffer b
  in
  add64 (String.length payload);
  add64 sec;
  add32 usec;
  add32 (Char.code kind);
  Buffer.add_string buffer payload;
  Buffer.contents buffer

(* A session whose third write redraws the second line rather than adding a
   third: the cursor goes up, returns to column zero and overwrites. Counting
   the lines in the byte stream reports three; the screen holds two. *)
let redraw_session =
  String.concat ""
    [
      record ~kind:'s' ~sec:100 ~usec:0 "\n";
      record ~kind:'o' ~sec:100 ~usec:0 "first\r\n";
      record ~kind:'o' ~sec:100 ~usec:500_000 "second\r\n";
      record ~kind:'o' ~sec:102 ~usec:250_000 "\027[1A\rthird \r\n";
    ]

let frames = Alcotest.(list (pair (float 1e-9) (list string)))

(* The screen after each write, with the seconds since the recording started.
   The header record makes no frame, the redraw replaces the row it landed on,
   and the space "third " left behind "second" is not part of what a reader
   sees. *)
let test_frames_are_the_screen_after_each_write () =
  Alcotest.check frames "three writes, two rows"
    [
      (0., [ "first" ]);
      (0.5, [ "first"; "second" ]);
      (2.25, [ "first"; "third" ]);
    ]
    (Console_vte.frames ~rows:6 ~cols:20 redraw_session)

(* Every screen carries the whole session, so the emulator is fed once rather
   than rebuilt per record: a fresh emulator per write would lose "first". *)
let test_frames_keep_the_screen_between_writes () =
  match Console_vte.frames ~rows:6 ~cols:20 redraw_session with
  | [ _; _; (_, rows) ] ->
      Alcotest.(check (list string))
        "the last screen still holds the first line" [ "first"; "third" ] rows
  | other -> Alcotest.failf "expected three frames, got %d" (List.length other)

(* A capture of a program that was still running ends mid-record. The stream
   stops there rather than raising, and every whole record before it stands. *)
let test_a_partial_record_ends_the_stream () =
  let truncated =
    let whole =
      redraw_session ^ record ~kind:'o' ~sec:103 ~usec:0 "never finished"
    in
    String.sub whole 0 (String.length whole - 4)
  in
  Alcotest.(check int)
    "the partial record makes no frame" 3
    (List.length (Console_vte.frames ~rows:6 ~cols:20 truncated));
  Alcotest.(check int)
    "and a header alone makes none either" 0
    (List.length
       (Console_vte.frames ~rows:6 ~cols:20
          (record ~kind:'s' ~sec:1 ~usec:0 "\n")))

(* A header whose length is near [max_int] promises more than the stream
   holds; the bound is checked without overflowing, so the stream ends. *)
let test_a_huge_length_ends_the_stream () =
  let huge =
    let header = record ~kind:'o' ~sec:1 ~usec:0 "" in
    let b = Bytes.of_string header in
    Bytes.set_int64_le b 0 (Int64.of_int max_int);
    Bytes.to_string b ^ "payload"
  in
  Alcotest.(check int) "no record" 0 (List.length (Console_vte.records huge))

(* The timestamp is seconds from the first record, which is what makes two
   captures of one command comparable; [records] carries the epoch seconds the
   capture stamped, which is what makes it a faithful read of the file. *)
let test_records_carry_the_wall_clock () =
  Alcotest.(check (list (pair (float 1e-9) string)))
    "three writes, stamped as recorded"
    [
      (100., "first\r\n"); (100.5, "second\r\n"); (102.25, "\027[1A\rthird \r\n");
    ]
    (Console_vte.records redraw_session)

(* [screen] is what a reader sees: no trailing blanks on a row, no blank rows
   under the last one, and the scrollback in front only when asked. *)
let test_screen_trims_what_no_reader_sees () =
  let vte = Vte.create ~rows:6 ~cols:20 () in
  Vte.feed_string vte "padded   \r\n";
  Alcotest.(check (list string))
    "the row is trimmed and the blank rows dropped" [ "padded" ]
    (Console_vte.screen vte)

let suite =
  ( "console_vte",
    [
      Alcotest.test_case "frames are the screen after each write" `Quick
        test_frames_are_the_screen_after_each_write;
      Alcotest.test_case "frames keep the screen between writes" `Quick
        test_frames_keep_the_screen_between_writes;
      Alcotest.test_case "a partial record ends the stream" `Quick
        test_a_partial_record_ends_the_stream;
      Alcotest.test_case "a huge length ends the stream" `Quick
        test_a_huge_length_ends_the_stream;
      Alcotest.test_case "records carry the wall clock" `Quick
        test_records_carry_the_wall_clock;
      Alcotest.test_case "screen trims what no reader sees" `Quick
        test_screen_trims_what_no_reader_sees;
    ] )
