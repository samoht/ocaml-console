(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let lines = Alcotest.(list string)

(* ---- Line accumulation, echo, submit ----------------------------------- *)

let test_printable () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc" in
  Alcotest.(check string) "echoes the typed bytes" "abc" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string)
    "pending holds the line" "abc" (Console_input.pending t)

let test_submit_cr () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc\r" in
  Alcotest.(check string) "echo ends in CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "one completed line" [ "abc" ] ls;
  Alcotest.(check string) "pending cleared" "" (Console_input.pending t)

let test_submit_lf () =
  let t = Console_input.v () in
  let _, ls = Console_input.feed t "abc\n" in
  Alcotest.(check lines) "LF submits too" [ "abc" ] ls

let test_crlf_is_one_line () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc\r\n" in
  Alcotest.(check string) "echo is a single CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "CRLF yields one line, no empty second" [ "abc" ] ls

let test_utf8_passthrough () =
  let t = Console_input.v () in
  let echo, _ = Console_input.feed t "caf\xc3\xa9" in
  Alcotest.(check string) "high bytes echoed verbatim" "caf\xc3\xa9" echo;
  Alcotest.(check string) "and stored" "caf\xc3\xa9" (Console_input.pending t)

(* ---- Backspace --------------------------------------------------------- *)

let test_backspace () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "ab\127c\r" in
  Alcotest.(check string)
    "erase echoes backspace-space-backspace" "ab\b \bc\r\n" echo;
  Alcotest.(check lines) "'b' erased, then 'c' appended" [ "ac" ] ls

let test_backspace_ctrl_h () =
  let t = Console_input.v () in
  let _, ls = Console_input.feed t "ab\008\r" in
  Alcotest.(check lines) "Ctrl-H also erases" [ "a" ] ls

let test_backspace_on_empty () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "\127" in
  Alcotest.(check string) "nothing to erase, no echo" "" echo;
  Alcotest.(check lines) "no line" [] ls

let test_control_chars_ignored () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "a\001\002b\r" in
  Alcotest.(check string)
    "control bytes neither echoed nor stored" "ab\r\n" echo;
  Alcotest.(check lines) "control bytes dropped" [ "ab" ] ls

let test_masked_input () =
  let t = Console_input.v ~mask:"*" () in
  let echo, ls = Console_input.feed t "s3cr3t" in
  Alcotest.(check string) "one mask per character" "******" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string)
    "secret retained for the caller" "s3cr3t" (Console_input.pending t);
  let echo, _ = Console_input.feed t "\127" in
  Alcotest.(check string) "backspace erases one mask" "\b \b" echo;
  let echo, ls = Console_input.feed t "\r" in
  Alcotest.(check string) "submit reveals nothing" "\r\n" echo;
  Alcotest.(check lines) "submitted value is unmasked" [ "s3cr3" ] ls;
  Alcotest.(check (list string))
    "secrets are never retained in history" [] (Console_input.history t)

(* ---- Feed boundaries --------------------------------------------------- *)

let test_split_across_feeds () =
  let t = Console_input.v () in
  let _, ls1 = Console_input.feed t "he" in
  Alcotest.(check lines) "nothing yet" [] ls1;
  let _, ls2 = Console_input.feed t "llo\r" in
  Alcotest.(check lines) "line accumulated across feeds" [ "hello" ] ls2

let test_split_crlf_across_feeds () =
  let t = Console_input.v () in
  let _, ls1 = Console_input.feed t "a\r" in
  Alcotest.(check lines) "CR submits" [ "a" ] ls1;
  let _, ls2 = Console_input.feed t "\nb\r" in
  Alcotest.(check lines) "trailing LF swallowed" [ "b" ] ls2

(* ---- Cursor movement and mid-line editing ------------------------------ *)

let left = "\027[D"
let right = "\027[C"

let test_cursor_insert () =
  (* "ab", Left (cursor before 'b'), insert 'X' -> "aXb" *)
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ "X") in
  Alcotest.(check string) "insert at the cursor" "aXb" (Console_input.pending t)

let test_cursor_backspace_midline () =
  (* "abc", Left (before 'c'), backspace deletes 'b' -> "ac" *)
  let t = Console_input.v () in
  let _ = Console_input.feed t ("abc" ^ left ^ "\127") in
  Alcotest.(check string)
    "backspace deletes before the cursor" "ac" (Console_input.pending t)

let test_cursor_right_returns_to_end () =
  (* "ab", Left then Right is back at the end, so 'X' appends -> "abX" *)
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ right ^ "X") in
  Alcotest.(check string)
    "right cancels the left" "abX" (Console_input.pending t)

let test_cursor_left_clamps () =
  (* Left past the start is a no-op; insert still goes at the front *)
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ left ^ left ^ "X") in
  Alcotest.(check string)
    "left clamps at the start" "Xab" (Console_input.pending t)

(* ---- UTF-8 ------------------------------------------------------------- *)

let test_utf8_backspace_whole_char () =
  (* backspace erases the whole two-byte 'e-acute', not one byte *)
  let t = Console_input.v () in
  let echo, _ = Console_input.feed t "caf\xc3\xa9\127" in
  Alcotest.(check string) "one cell erased" "caf\xc3\xa9\b \b" echo;
  Alcotest.(check string) "whole character gone" "caf" (Console_input.pending t)

let test_utf8_split_across_feeds () =
  (* the two bytes of a character arrive in separate feeds *)
  let t = Console_input.v () in
  let _, _ = Console_input.feed t "\xc3" in
  Alcotest.(check string)
    "incomplete character not yet in the line" "" (Console_input.pending t);
  let _, _ = Console_input.feed t "\xa9" in
  Alcotest.(check string)
    "completed once its bytes arrive" "\xc3\xa9" (Console_input.pending t)

(* ---- Wide characters ---------------------------------------------------- *)

(* A character two cells wide drives the cursor motion and the erase by its
   cells, not by the character count. *)
let test_wide_backspace_clears_two_cells () =
  let echo, _ = Console_input.feed (Console_input.v ()) "\xe4\xb8\xad\127" in
  Alcotest.(check string) "two cells cleared" "\xe4\xb8\xad\b\b  \b\b" echo

let test_wide_left_steps_two_cells () =
  let echo, _ =
    Console_input.feed (Console_input.v ())
      ("\xe4\xb8\xad\xe6\x96\x87" ^ "\027[D")
  in
  Alcotest.(check string)
    "left steps over two cells" "\xe4\xb8\xad\xe6\x96\x87\b\b" echo

(* ---- History ----------------------------------------------------------- *)

let up = "\027[A"
let down = "\027[B"

let test_history_up () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "abc\r" in
  let echo, ls = Console_input.feed t up in
  Alcotest.(check string)
    "recalled line echoed (empty buffer, no erase)" "abc" echo;
  Alcotest.(check lines) "no submit" [] ls;
  Alcotest.(check string)
    "pending is the recalled line" "abc" (Console_input.pending t)

let test_history_up_twice_clamps () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "one\r" in
  let _ = Console_input.feed t "two\r" in
  let _ = Console_input.feed t up in
  Alcotest.(check string) "first up = newest" "two" (Console_input.pending t);
  let _ = Console_input.feed t up in
  Alcotest.(check string) "second up = older" "one" (Console_input.pending t);
  let _ = Console_input.feed t up in
  Alcotest.(check string)
    "third up clamps at oldest" "one" (Console_input.pending t)

let test_history_down_restores_draft () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "old\r" in
  let _ = Console_input.feed t "dra" in
  let _ = Console_input.feed t up in
  Alcotest.(check string) "up recalls history" "old" (Console_input.pending t);
  let _ = Console_input.feed t down in
  Alcotest.(check string)
    "down restores the in-progress draft" "dra" (Console_input.pending t)

let test_history_erase_on_recall () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "abcd\r" in
  let _ = Console_input.feed t "xy" in
  let echo, _ = Console_input.feed t up in
  Alcotest.(check string)
    "erases the two draft chars then writes recalled line" "\b \b\b \babcd" echo

let test_history_dedup_consecutive () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "x\r" in
  let _ = Console_input.feed t "x\r" in
  Alcotest.(check lines)
    "consecutive duplicate kept once" [ "x" ] (Console_input.history t)

let test_history_seed_and_order () =
  let t = Console_input.v ~history:[ "first"; "second" ] () in
  let _ = Console_input.feed t "third\r" in
  Alcotest.(check lines)
    "oldest first, then new"
    [ "first"; "second"; "third" ]
    (Console_input.history t)

(* ---- Tab completion ---------------------------------------------------- *)

let test_complete_single () =
  let t = Console_input.v ~complete:(fun _ -> [ "select" ]) () in
  let _ = Console_input.feed t "sel" in
  let echo, _ = Console_input.feed t "\t" in
  Alcotest.(check string) "tab echoes only the completion suffix" "ect" echo;
  Alcotest.(check string) "pending completed" "select" (Console_input.pending t)

let test_complete_common_prefix () =
  let t = Console_input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let _ = Console_input.feed t "co\t" in
  Alcotest.(check string)
    "extends to longest common prefix" "con" (Console_input.pending t)

let test_complete_lists_candidates () =
  let t = Console_input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let echo, _ = Console_input.feed t "co\t" in
  Alcotest.(check bool)
    "lists both candidates" true
    (Display_harness.contains "connections  config" echo)

let test_complete_last_word () =
  let complete w = if String.equal w "co" then [ "connections" ] else [] in
  let t = Console_input.v ~complete () in
  let _ = Console_input.feed t "SELECT * FROM co\t" in
  Alcotest.(check string)
    "completes only the trailing word" "SELECT * FROM connections"
    (Console_input.pending t)

let test_complete_none () =
  let t = Console_input.v ~complete:(fun _ -> []) () in
  let _ = Console_input.feed t "zz" in
  let echo, _ = Console_input.feed t "\t" in
  Alcotest.(check string) "no candidates, tab echoes nothing" "" echo;
  Alcotest.(check string) "pending unchanged" "zz" (Console_input.pending t)

let test_complete_utf8 () =
  let t = Console_input.v ~complete:(fun _ -> [ "é"; "ê" ]) () in
  ignore (Console_input.feed t "\t");
  Alcotest.(check string)
    "no complete character is shared" "" (Console_input.pending t)

(* A byte sequence that is not a character is one replacement character, in
   the line and on the screen, so the cursor stays where the terminal puts
   it. *)
let test_invalid_utf8 () =
  List.iter
    (fun bad ->
      let t = Console_input.v () in
      let echo, _ = Console_input.feed t bad in
      Alcotest.(check string)
        (String.escaped bad) "\u{FFFD}" (Console_input.pending t);
      Alcotest.(check string) "echo" "\u{FFFD}" echo)
    [ "\xed\xa0\x80"; "\xc0\xaf"; "\xf4\x90\x80\x80" ]

let test_echo_is_text () =
  let no_escape msg (echo, _) =
    Alcotest.(check bool) msg false (String.contains echo '\027')
  in
  let t =
    Console_input.v ~prompt:"\027[2J> " ~history:[ "\027]0;owned\007" ]
      ~complete:(fun _ -> [ "a\027[31m"; "ab\027[2J" ])
      ()
  in
  no_escape "completion and prompt" (Console_input.feed t "\t");
  no_escape "history" (Console_input.feed t "\027[A")

(* A typed character the terminal would act on, such as the one-byte CSI
   U+009B, is echoed and kept as a space. *)
let test_typed_c1 () =
  let t = Console_input.v () in
  let echo, submitted = Console_input.feed t "a\xc2\x9b2Jb\r" in
  Alcotest.(check bool)
    "no C1 control echoed" false
    (Display_harness.contains "\xc2\x9b" echo);
  Alcotest.check lines "line" [ "a 2Jb" ] submitted

(* A terminal in application cursor mode sends the arrows as ESC O A to
   ESC O D; they move as ESC [ A to ESC [ D do and insert nothing. *)
let test_application_arrows () =
  let t = Console_input.v ~history:[ "old" ] () in
  let _, submitted = Console_input.feed t "ab\027ODX\r" in
  Alcotest.check lines "left arrow" [ "aXb" ] submitted;
  let _, submitted = Console_input.feed t "\027OA\r" in
  Alcotest.check lines "up arrow" [ "aXb" ] submitted

let suite =
  ( "console_input",
    [
      Alcotest.test_case "typed C1 control is a space" `Quick test_typed_c1;
      Alcotest.test_case "application mode arrows" `Quick
        test_application_arrows;
      Alcotest.test_case "invalid UTF-8 is a replacement character" `Quick
        test_invalid_utf8;
      Alcotest.test_case "echo holds no escape from the caller" `Quick
        test_echo_is_text;
      Alcotest.test_case "completion inserts only complete UTF-8 characters"
        `Quick test_complete_utf8;
      Alcotest.test_case "printable" `Quick test_printable;
      Alcotest.test_case "submit on CR" `Quick test_submit_cr;
      Alcotest.test_case "submit on LF" `Quick test_submit_lf;
      Alcotest.test_case "CRLF is one line" `Quick test_crlf_is_one_line;
      Alcotest.test_case "UTF-8 passthrough" `Quick test_utf8_passthrough;
      Alcotest.test_case "backspace" `Quick test_backspace;
      Alcotest.test_case "Ctrl-H backspace" `Quick test_backspace_ctrl_h;
      Alcotest.test_case "backspace on empty" `Quick test_backspace_on_empty;
      Alcotest.test_case "control chars ignored" `Quick
        test_control_chars_ignored;
      Alcotest.test_case "masked secret input" `Quick test_masked_input;
      Alcotest.test_case "split across feeds" `Quick test_split_across_feeds;
      Alcotest.test_case "split CRLF across feeds" `Quick
        test_split_crlf_across_feeds;
      Alcotest.test_case "cursor insert" `Quick test_cursor_insert;
      Alcotest.test_case "cursor backspace mid-line" `Quick
        test_cursor_backspace_midline;
      Alcotest.test_case "cursor right returns to end" `Quick
        test_cursor_right_returns_to_end;
      Alcotest.test_case "cursor left clamps" `Quick test_cursor_left_clamps;
      Alcotest.test_case "UTF-8 backspace whole char" `Quick
        test_utf8_backspace_whole_char;
      Alcotest.test_case "UTF-8 split across feeds" `Quick
        test_utf8_split_across_feeds;
      Alcotest.test_case "wide char backspace" `Quick
        test_wide_backspace_clears_two_cells;
      Alcotest.test_case "wide char left" `Quick test_wide_left_steps_two_cells;
      Alcotest.test_case "history up" `Quick test_history_up;
      Alcotest.test_case "history up clamps" `Quick test_history_up_twice_clamps;
      Alcotest.test_case "history down restores draft" `Quick
        test_history_down_restores_draft;
      Alcotest.test_case "history erases on recall" `Quick
        test_history_erase_on_recall;
      Alcotest.test_case "history dedup consecutive" `Quick
        test_history_dedup_consecutive;
      Alcotest.test_case "history seed and order" `Quick
        test_history_seed_and_order;
      Alcotest.test_case "complete single" `Quick test_complete_single;
      Alcotest.test_case "complete common prefix" `Quick
        test_complete_common_prefix;
      Alcotest.test_case "complete lists candidates" `Quick
        test_complete_lists_candidates;
      Alcotest.test_case "complete last word" `Quick test_complete_last_word;
      Alcotest.test_case "complete none" `Quick test_complete_none;
    ] )
