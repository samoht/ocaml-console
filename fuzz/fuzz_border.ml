(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* [chars] and [style] give back what [v] and [with_style] were given. *)
let test s =
  let chars =
    {
      Border.top_left = s;
      top = s;
      top_right = s;
      left = s;
      right = s;
      bottom_left = s;
      bottom = s;
      bottom_right = s;
      cross = s;
      top_cross = s;
      bottom_cross = s;
      left_cross = s;
      right_cross = s;
    }
  in
  let b = Border.v ~style:Style.bold chars in
  check (Border.chars b = chars && Style.equal (Border.style b) Style.bold);
  List.iter
    (fun b ->
      let styled = Border.with_style Style.italic b in
      check (Border.chars styled = Border.chars b);
      check (Style.equal (Border.style styled) Style.italic);
      ignore (Fmt.str "%a" Border.pp b))
    [
      b;
      Border.none;
      Border.ascii;
      Border.single;
      Border.double;
      Border.rounded;
      Border.heavy;
      Border.hidden;
    ]

let suite = ("border", [ test_case "accessors" [ bytes ] test ])
