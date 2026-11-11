(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Recording = Console__.Recording

let with_env = Env_harness.with_env
let read file = In_channel.with_open_bin file In_channel.input_all

let with_temp f =
  let file = Filename.temp_file "console-test-recording" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () -> f file)

(* Ends the recording under way, which puts its formatters back. *)
let end_recording () =
  with_env
    [ ("CONSOLE_CAST", "") ]
    (fun () ->
      let unused = Format.formatter_of_buffer (Buffer.create 0) in
      ignore (Recording.start ~time:(fun () -> 0.) unused : bool))

(* [recording f] runs [f file] with a recording asked for on a temporary
   [file] and a known terminal, then leaves no recording, no file and no
   variable changed. *)
let recording ?(env = []) f =
  with_temp (fun file ->
      with_env
        ([
           ("CONSOLE_CAST", file);
           ("TERM", "xterm");
           ("COLUMNS", "");
           ("LINES", "");
         ]
        @ env)
        (fun () -> Fun.protect ~finally:end_recording (fun () -> f file)))

let started ?(time = fun () -> 0.) ppf =
  Alcotest.(check bool) "recording" true (Recording.start ~time ppf)

let buffered () =
  let b = Buffer.create 16 in
  (b, Format.formatter_of_buffer b)

(* The lines of a finished recording. *)
let lines file = List.filter (( <> ) "") (String.split_on_char '\n' (read file))
let header = {|{"version":2,"width":80,"height":24,"env":{"TERM":"xterm"}}|}

(* Lines are compared as the JSON values they write: asciicast v2 is
   newline-delimited JSON, whatever its spacing or number format. *)
let check_lines msg expected actual =
  Alcotest.(check (list Cast_harness.json))
    msg
    (List.map Cast_harness.parse expected)
    (List.map Cast_harness.parse actual)

let write ppf s =
  Format.pp_print_string ppf s;
  Format.pp_print_flush ppf ()

(* A clock that reads the given times in turn, and stays on the last. *)
let clock times =
  let left = ref times in
  fun () ->
    match !left with
    | [] -> 0.
    | [ t ] -> t
    | t :: rest ->
        left := rest;
        t

let test_size () =
  let check label expected (columns, lines) =
    with_env
      [ ("COLUMNS", columns); ("LINES", lines) ]
      (fun () ->
        Alcotest.(check (pair int int)) label expected (Recording.size ()))
  in
  check "empty is the default" (80, 24) ("", "");
  check "both read" (132, 43) ("132", "43");
  check "lowest" (1, 1) ("1", "1");
  check "highest" (4096, 4096) ("4096", "4096");
  check "zero" (80, 24) ("0", "0");
  check "above" (80, 24) ("4097", "4097");
  check "negative" (80, 24) ("-1", "-5");
  check "each on its own: columns" (100, 24) ("100", "0");
  check "each on its own: lines" (80, 50) ("x", "50");
  check "not a number" (80, 24) ("wide", "tall");
  check "a number and more" (80, 24) ("100x", "50 ");
  check "a fraction" (80, 24) ("100.5", "1e2");
  check "beyond an int" (80, 24)
    ("99999999999999999999", "-99999999999999999999");
  check "max_int" (80, 24) (string_of_int max_int, string_of_int max_int);
  check "invalid bytes" (80, 24) ("\xff\xfe", "\027[31m");
  check "hexadecimal" (80, 24) ("0x50", "0x18");
  check "octal and binary" (80, 24) ("0o17", "0b11");
  check "unsigned" (80, 24) ("0u20", "0u20");
  check "underscores" (80, 24) ("1_000", "1_0");
  check "a sign" (80, 24) ("+5", "+5")

let test_not_asked () =
  with_env
    [ ("CONSOLE_CAST", "") ]
    (fun () ->
      let b, ppf = buffered () in
      Alcotest.(check bool)
        "an empty name" false
        (Recording.start ~time:(fun () -> 0.) ppf);
      write ppf "still here";
      Alcotest.(check string)
        "the formatter is its own" "still here" (Buffer.contents b))

let test_header () =
  let check label env expected =
    recording ~env (fun file ->
        started (snd (buffered ()));
        check_lines label [ expected ] (lines file))
  in
  check "the default size" [] header;
  check "the size asked"
    [ ("COLUMNS", "132"); ("LINES", "43") ]
    {|{"version":2,"width":132,"height":43,"env":{"TERM":"xterm"}}|};
  check "a size out of range" [ ("COLUMNS", "4097"); ("LINES", "0") ] header;
  check "an empty TERM is no TERM"
    [ ("TERM", "") ]
    {|{"version":2,"width":80,"height":24}|};
  check "a TERM that is not text is still one line"
    [ ("TERM", "a\"\n\027") ]
    {|{"version":2,"width":80,"height":24,"env":{"TERM":"a\"\n\u001B"}}|};
  (* RFC 8259 section 8.1: JSON text exchanged is UTF-8, whatever the bytes of
     the variable. A lead byte at the end of the value must not swallow the
     closing quote. Windows stores the environment in UTF-16 and converts
     bytes that are not UTF-8 through its code page, so it cannot hold these
     bytes. *)
  if not Sys.win32 then
    List.iter
      (fun term ->
        check
          ("a TERM that is not UTF-8: " ^ String.escaped term)
          [ ("TERM", term) ]
          (Fmt.str {|{"version":2,"width":80,"height":24,"env":{"TERM":"%s"}}|}
             "\u{FFFD}"))
      [ "\xc2"; "\xff"; "\xe2\x82" ]

let test_events () =
  recording (fun file ->
      let b, ppf = buffered () in
      Format.pp_print_string ppf "before";
      started ~time:(clock [ 1.5; 2.25; 3.0 ]) ppf;
      Alcotest.(check string)
        "what was waiting goes where it was going" "before" (Buffer.contents b);
      write ppf "one";
      write ppf "two";
      Fmt.pf ppf "%d %s@." 3 "three";
      Alcotest.(check string)
        "nothing more reaches the formatter's output" "before"
        (Buffer.contents b);
      check_lines "one event a flush"
        [
          header;
          {|[1.5,"o","one"]|};
          {|[2.25,"o","two"]|};
          {|[3,"o","3 three\r\n"]|};
        ]
        (lines file))

let test_flush_of_nothing () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      Format.pp_print_flush ppf ();
      Format.pp_print_flush ppf ();
      check_lines "no empty event" [ header ] (lines file))

let test_boxes () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      Fmt.pf ppf "@[<v 2>a@,b@]%!";
      check_lines "newlines and indents are written"
        [ header; {|[0,"o","a\r\n  b"]|} ]
        (lines file))

(* A terminal's driver puts a carriage return before a line feed that has
   none. *)
let test_carriage_returns () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      write ppf "\n";
      write ppf "a\nb\r\nc\n\n";
      write ppf "cut\r";
      write ppf "\nnext\r";
      write ppf "x\n";
      check_lines "as a terminal shows them"
        [
          header;
          {|[0,"o","\r\n"]|};
          {|[0,"o","a\r\nb\r\nc\r\n\r\n"]|};
          {|[0,"o","cut\r"]|};
          {|[0,"o","\nnext\r"]|};
          {|[0,"o","x\r\n"]|};
        ]
        (lines file))

(* The time never goes back, and it is written to the microsecond. *)
let test_time () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ~time:(clock [ -1.; 5.; 2.; 5.1234567; 5.1234561 ]) ppf;
      List.iter (write ppf) [ "a"; "b"; "c"; "d"; "e" ];
      check_lines "times"
        [
          header;
          {|[0,"o","a"]|};
          {|[5,"o","b"]|};
          {|[5,"o","c"]|};
          {|[5.123457,"o","d"]|};
          {|[5.123457,"o","e"]|};
        ]
        (lines file))

(* A character or an escape code cut by a flush waits for the rest. *)
let test_cut_short () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      write ppf "\xc3";
      check_lines "half a character is not written" [ header ] (lines file);
      write ppf "\xa9";
      write ppf "a\xe2\x9d";
      write ppf "\xa4";
      write ppf "red\027[3";
      write ppf "1mx";
      check_lines "each piece whole"
        [
          header;
          "[0,\"o\",\"\u{e9}\"]";
          {|[0,"o","a"]|};
          "[0,\"o\",\"\u{2764}\"]";
          {|[0,"o","red"]|};
          {|[0,"o","\u001B[31mx"]|};
        ]
        (lines file))

(* asciicast v2, "o": data is valid UTF-8. Bytes that are not UTF-8 are
   replaced as Unicode Core Spec 3.9 says, and the bytes of a character left
   unfinished are written, replaced, when the recording ends. *)
let test_hostile_bytes () =
  let replaced = "\u{fffd}" in
  with_temp (fun other ->
      recording (fun file ->
          let _, ppf = buffered () in
          started ppf;
          write ppf "\xff\xfea\000b\"\\";
          write ppf "\xc3";
          with_env
            [ ("CONSOLE_CAST", other) ]
            (fun () -> started (snd (buffered ())));
          let line = Fmt.str "[0,\"o\",\"%s\"]" in
          check_lines "replaced"
            [
              header;
              line (replaced ^ replaced ^ {|a\u0000b\"\\|});
              line replaced;
            ]
            (lines file)))

(* asciicast v2, "o": non-printable code points are encoded as \\uXXXX. Here
   DEL and the C1 control NEL, which JSON itself would let through. The
   lines are compared as text: as JSON values, an escaped NEL is NEL. *)
let test_non_printable () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      write ppf "a\x7f\xc2\x85b";
      Alcotest.(check (list string))
        "escaped"
        [ header; {|[0,"o","a\u007F\u0085b"]|} ]
        (lines file))

(* Each maximal subpart is one U+FFFD, not each ill-formed sequence. *)
let test_maximal_subparts () =
  List.iter
    (fun (table, bytes, converted) ->
      recording (fun file ->
          let _, ppf = buffered () in
          started ppf;
          write ppf bytes;
          Alcotest.(check (list string))
            table [ converted ]
            (List.map
               (fun (e : Cast_harness.event) -> e.data)
               (Cast_harness.events (lines file)))))
    Text_harness.maximal_subparts

let test_large_write () =
  recording (fun file ->
      let _, ppf = buffered () in
      started ppf;
      let large = String.make (1024 * 1024) 'x' in
      write ppf large;
      check_lines "one event, whole"
        [ header; Fmt.str {|[0,"o","%s"]|} large ]
        (lines file))

(* A second start on the same file goes on with the recording: no second
   header, the new clock, a time that still does not go back, and a formatter
   seen twice recorded once. *)
let test_same_file () =
  recording (fun file ->
      let _, first = buffered () in
      started ~time:(fun () -> 7.) first;
      write first "a";
      started ~time:(fun () -> 1.) first;
      write first "b";
      let _, second = buffered () in
      started ~time:(fun () -> 9.) second;
      write second "c";
      write first "d";
      check_lines "one recording"
        [
          header;
          {|[7,"o","a"]|};
          {|[7,"o","b"]|};
          {|[9,"o","c"]|};
          {|[9,"o","d"]|};
        ]
        (lines file))

(* Another file ends the recording: what was waiting is in it, its formatter
   writes where it did, and the new one has its own time, from zero. *)
let test_other_file () =
  with_temp (fun other ->
      recording (fun file ->
          let b, first = buffered () in
          started ~time:(fun () -> 7.) first;
          write first "a";
          Format.pp_print_string first "waiting";
          let _, second = buffered () in
          with_env
            [ ("CONSOLE_CAST", other) ]
            (fun () ->
              started ~time:(fun () -> 1.) second;
              write second "b";
              write first "back";
              Alcotest.(check string)
                "the first formatter writes where it did" "back"
                (Buffer.contents b);
              check_lines "the first recording"
                [ header; {|[7,"o","a"]|}; {|[7,"o","waiting"]|} ]
                (lines file);
              check_lines "the second recording"
                [ header; {|[1,"o","b"]|} ]
                (lines other))))

(* No recording asked for ends the last one too. *)
let test_end () =
  recording (fun file ->
      let b, ppf = buffered () in
      started ppf;
      write ppf "in";
      Alcotest.(check bool)
        "no recording" false
        (with_env
           [ ("CONSOLE_CAST", "") ]
           (fun () -> Recording.start ~time:(fun () -> 0.) ppf));
      write ppf "out";
      Alcotest.(check string) "written as before" "out" (Buffer.contents b);
      check_lines "written as before, not recorded"
        [ header; {|[0,"o","in"]|} ]
        (lines file))

let out_string ppf = (Format.pp_get_formatter_out_functions ppf ()).out_string

let test_standard_formatters () =
  let was_out = out_string Fmt.stdout and was_err = out_string Fmt.stderr in
  let same = Alcotest.(check bool) in
  let check label ppf =
    recording (fun file ->
        Format.pp_print_flush Fmt.stdout ();
        Format.pp_print_flush Fmt.stderr ();
        (* Alcotest prints on the standard formatters: no check until they are
           back. *)
        let asked = Recording.start ~time:(fun () -> 0.) ppf in
        write Fmt.stdout "out";
        write Fmt.stderr "err";
        let recorded = lines file in
        end_recording ();
        same (label ^ ": recording") true asked;
        same (label ^ ": stdout back") true (out_string Fmt.stdout == was_out);
        same (label ^ ": stderr back") true (out_string Fmt.stderr == was_err);
        check_lines label
          [ header; {|[0,"o","out"]|}; {|[0,"o","err"]|} ]
          recorded)
  in
  check "from stdout" Fmt.stdout;
  check "from stderr" Fmt.stderr;
  (* Any other formatter leaves the standard ones alone. *)
  recording (fun _ ->
      started (snd (buffered ()));
      same "stdout left alone" true (out_string Fmt.stdout == was_out);
      same "stderr left alone" true (out_string Fmt.stderr == was_err))

(* [start] ends the program on a file it cannot open, so it runs in a child;
   what the child does to its environment and formatters stays there. Windows
   has no [fork]. *)
let cannot_record ~before file =
  if Sys.win32 then Alcotest.skip ();
  let log = Filename.temp_file "console-test-recording" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove log) @@ fun () ->
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush_all ();
  match Unix.fork () with
  | 0 ->
      let out = Unix.openfile log [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
      Unix.dup2 out Unix.stderr;
      before ();
      Unix.putenv "CONSOLE_CAST" file;
      ignore (Recording.start ~time:(fun () -> 0.) Fmt.stdout : bool);
      (* [start] was to exit: this is its failure to. *)
      Unix._exit 2
  | child ->
      let _, status = Unix.waitpid [] child in
      let code = match status with Unix.WEXITED n -> n | _ -> -1 in
      (code, read log)

let cannot_open () =
  let check label ~error file =
    let code, message = cannot_record ~before:ignore file in
    Alcotest.(check int) (label ^ ": exit code") 1 code;
    Alcotest.(check string)
      (label ^ ": why")
      (Fmt.str "%s: CONSOLE_CAST: cannot record: %s: %s\n"
         (Filename.basename Sys.executable_name)
         file error)
      message
  in
  with_temp (fun file ->
      check "under a file" ~error:"Not a directory" (Filename.concat file "x"));
  let missing = Filename.temp_file "console-test-recording" ".dir" in
  Sys.remove missing;
  check "in no directory" ~error:"No such file or directory"
    (Filename.concat missing "run.cast");
  check "a directory" ~error:"Is a directory" (Filename.get_temp_dir_name ())

(* The recording under way is whole before the program ends. *)
let cannot_open_during_a_recording () =
  with_temp (fun bad_parent ->
      with_temp (fun file ->
          let child () =
            Unix.putenv "CONSOLE_CAST" file;
            Unix.putenv "TERM" "xterm";
            Unix.putenv "COLUMNS" "";
            Unix.putenv "LINES" "";
            let _, ppf = buffered () in
            ignore (Recording.start ~time:(fun () -> 2.) ppf : bool);
            write ppf "written";
            Format.pp_print_string ppf "waiting"
          in
          let code, _ =
            cannot_record ~before:child (Filename.concat bad_parent "x")
          in
          Alcotest.(check int) "exit code" 1 code;
          check_lines "the last recording ended"
            [ header; {|[2,"o","written"]|}; {|[2,"o","waiting"]|} ]
            (lines file)))

let suite =
  ( "recording",
    [
      Alcotest.test_case "size" `Quick test_size;
      Alcotest.test_case "not asked for" `Quick test_not_asked;
      Alcotest.test_case "header" `Quick test_header;
      Alcotest.test_case "events" `Quick test_events;
      Alcotest.test_case "a flush of nothing" `Quick test_flush_of_nothing;
      Alcotest.test_case "boxes" `Quick test_boxes;
      Alcotest.test_case "carriage returns" `Quick test_carriage_returns;
      Alcotest.test_case "time" `Quick test_time;
      Alcotest.test_case "cut short" `Quick test_cut_short;
      Alcotest.test_case "hostile bytes" `Quick test_hostile_bytes;
      Alcotest.test_case "non-printable code points" `Quick test_non_printable;
      Alcotest.test_case "maximal subparts" `Quick test_maximal_subparts;
      Alcotest.test_case "a large write" `Quick test_large_write;
      Alcotest.test_case "the same file goes on" `Quick test_same_file;
      Alcotest.test_case "another file ends it" `Quick test_other_file;
      Alcotest.test_case "none ends it" `Quick test_end;
      Alcotest.test_case "standard formatters" `Quick test_standard_formatters;
      Alcotest.test_case "a file that cannot be opened" `Quick cannot_open;
      Alcotest.test_case "cannot open during a recording" `Quick
        cannot_open_during_a_recording;
    ] )
