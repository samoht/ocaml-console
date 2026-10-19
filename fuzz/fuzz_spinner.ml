(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* A spinner shows one of its frames, as the text it prints, at any time, the first one before time
   starts or when it is not finite; an empty spinner and a non-positive or
   non-finite rate are refused. *)
let test_frame frames elapsed fps =
  (match Spinner.v [||] with
  | _ -> fail "empty spinner accepted"
  | exception Invalid_argument _ -> ());
  let t = Spinner.v frames in
  let frames = Array.map (Console_text.sanitize ~keep_newlines:false) frames in
  let fps_ok = Float.is_finite fps && fps > 0. in
  match Spinner.frame ~fps t ~elapsed with
  | f ->
      if not fps_ok then fail "bad rate accepted";
      check (Array.exists (String.equal f) frames);
      if (not (Float.is_finite elapsed)) || elapsed <= 0. then
        check (String.equal f frames.(0))
  | exception Invalid_argument _ -> if fps_ok then fail "rate refused"

let suite =
  ("spinner", [ test_case "frame" [ array1 bytes; float; float ] test_frame ])
