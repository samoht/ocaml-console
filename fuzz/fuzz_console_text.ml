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

let suite =
  ( "console_text",
    [
      test_case "sanitize and strip" [ bytes ] test_laws;
      test_case "sanitize_styles leaves nothing on" [ bytes ]
        test_styles_isolated;
    ] )
