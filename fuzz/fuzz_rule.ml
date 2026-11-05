(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console
open Widget_harness

(* A rule is as wide as the margin, and a glyph that is not one cell wide
   once sanitized is refused. *)
let test_rule s n =
  let margin = 1 + (abs n mod 80) in
  let glyph = pick s 0 and label = View.text (pick s 1) in
  match Rule.view ~glyph ~label () with
  | r ->
      let raw, lines = render View.pp margin r in
      check_closed raw;
      check_eq ~pp:Fmt.int margin (cells (List.hd lines))
  | exception Invalid_argument _ ->
      check (cells (Console_text.sanitize ~keep_newlines:false glyph) <> 1)

let suite =
  ("rule", [ test_case "as wide as the margin" [ bytes; int ] test_rule ])
