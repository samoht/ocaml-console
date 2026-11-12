(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* The accessors return what [v] and [with_style] received. Each glyph comes
   back as the text it prints. *)
let test a b c d =
  let g = Guide.v ~branch:a ~last:b ~pipe:c ~space:d () in
  let printed = Console_text.sanitize ~keep_newlines:false in
  check
    (Guide.branch g = printed a
    && Guide.last g = printed b
    && Guide.pipe g = printed c
    && Guide.space g = printed d);
  let styled = Guide.with_style Style.bold g in
  check
    (Style.equal (Guide.style styled) Style.bold
    && Guide.pipe styled = Guide.pipe g);
  Fmt.kstr ignore "%a" Guide.pp g

let suite =
  ("guide", [ test_case "accessors" [ bytes; bytes; bytes; bytes ] test ])
