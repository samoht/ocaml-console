(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let check msg expected s =
  Alcotest.(check int) msg expected (Width.string_width s)

let test_widths () =
  check "ASCII" 5 "hello";
  check "empty" 0 "";
  check "malformed UTF-8 is one replacement cell" 1 "\xff";
  check "a spacing mark attaches to its letter" 1 "x\xe0\xa5\x80";
  check "a combining mark adds nothing" 1 "e\xcc\x81";
  check "CJK" 4 "\xe7\x95\x8c\xe7\x95\x8c";
  check "emoji" 2 "\xf0\x9f\x8e\x89";
  check "ZWJ family" 2
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7";
  check "flag" 2 "\xf0\x9f\x87\xab\xf0\x9f\x87\xb7";
  check "keycap" 2 "1\xef\xb8\x8f\xe2\x83\xa3";
  check "VS16 makes an emoji" 2 "\xe2\x9d\xa4\xef\xb8\x8f"

(* The width is the column a terminal's cursor reaches. *)
let test_matches_terminal () =
  List.iter
    (fun s ->
      let vte = Vte.create ~rows:1 ~cols:80 () in
      Vte.feed_string vte s;
      Alcotest.(check int)
        (String.escaped s)
        (snd (Vte.cursor_pos vte))
        (Width.string_width s))
    [ "hello"; "\xe7\x95\x8c\xe7\x95\x8c"; "e\xcc\x81"; "\xf0\x9f\x8e\x89 ok" ]

let suite =
  ( "width",
    [
      Alcotest.test_case "widths" `Quick test_widths;
      Alcotest.test_case "matches the terminal" `Quick test_matches_terminal;
    ] )
