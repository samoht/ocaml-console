(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* For printable UTF-8, the width is the column a terminal's cursor reaches
   after the same bytes. *)
let test_matches_terminal s =
  let s = Console_text.sanitize ~keep_newlines:false s in
  guard (Width.string_width s < 1000);
  let vte = Vte.create ~rows:1 ~cols:1000 () in
  Vte.feed_string vte s;
  check_eq ~pp:Fmt.int (snd (Vte.cursor_pos vte)) (Width.string_width s)

let suite =
  ("width", [ test_case "matches the terminal" [ bytes ] test_matches_terminal ])
