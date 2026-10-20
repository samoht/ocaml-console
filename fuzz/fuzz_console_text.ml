(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let printable s =
  String.is_valid_utf_8 s
  && not
       (String.exists
          (fun c -> (Char.code c < 0x20 && c <> '\n') || c = '\127')
          s)

(* [sanitize] and [strip] give printable UTF-8 and are idempotent; [strip]
   leaves no escape; [sanitize_styles] leaves nothing on after it: text
   printed after it is drawn as it would be on its own. *)
let test_laws s =
  let san = Console_text.sanitize s and str = Console_text.strip s in
  check (printable san && printable str);
  check (not (String.contains str '\027'));
  check_eq ~pp:Fmt.Dump.string san (Console_text.sanitize san);
  check_eq ~pp:Fmt.Dump.string str (Console_text.strip str);
  check
    (not (String.contains (Console_text.sanitize ~keep_newlines:false s) '\n'))

let test_styles_isolated s =
  let draw prefix =
    let vte = Vte.create ~rows:2 ~cols:8 () in
    Vte.feed_string vte ("\027[2J\027[H" ^ prefix ^ "\r\n!");
    let g = Vte.grid vte in
    let i = Grid.idx g ~x:0 ~y:1 in
    (Grid.get_fg_r g i, Grid.get_fg_g g i, Grid.get_fg_b g i, Grid.get_bg_r g i)
  in
  let styled = Console_text.sanitize_styles ~keep_newlines:false s in
  check (draw styled = draw "")

(* A line ends at LF, VT, FF, at NEL (U+0085, which UTF-8 writes C2 85),
   and at a CR that none of them follows: [sanitize] keeps one line feed for
   each. *)
let line_ends s =
  let n = String.length s in
  let nel i = i + 1 < n && s.[i] = '\xc2' && s.[i + 1] = '\x85' in
  let feed i = i < n && (String.contains "\n\011\012" s.[i] || nel i) in
  let count = ref 0 in
  String.iteri
    (fun i c -> if feed i || (c = '\r' && not (feed (i + 1))) then incr count)
    s;
  !count

let test_line_ends s =
  let lines = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 in
  check_eq ~pp:Fmt.int (line_ends s) (lines (Console_text.sanitize s))

(* CAN and SUB print nothing: put between two characters of text that holds
   no escape, they leave it as it was. *)
let test_cancel_discarded s p sub =
  let s = Console_text.strip s in
  let rec boundary i =
    if i >= String.length s then String.length s
    else if Uchar.utf_decode_is_valid (String.get_utf_8_uchar s i) then i
    else boundary (i + 1)
  in
  let i = boundary (if s = "" then 0 else p mod (String.length s + 1)) in
  let c = if sub then "\026" else "\024" in
  let with_c = String.sub s 0 i ^ c ^ String.sub s i (String.length s - i) in
  check_eq ~pp:Fmt.Dump.string s (Console_text.strip with_c)

let suite =
  ( "console_text",
    [
      test_case "sanitize and strip" [ bytes ] test_laws;
      test_case "one line feed per line end" [ bytes ] test_line_ends;
      test_case "CAN and SUB print nothing" [ bytes; uint8; bool ]
        test_cancel_discarded;
      test_case "sanitize_styles leaves nothing on" [ bytes ]
        test_styles_isolated;
    ] )
