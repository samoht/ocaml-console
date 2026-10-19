(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Feeding input at once or a byte at a time echoes and submits the same; a
   masked editor keeps no history; the echo never holds an escape the input
   did not ask for. *)
let test_chunks s =
  let make () =
    Input.v ~prompt:s ~history:[ s ]
      ~complete:(fun word -> [ word; word ^ s ])
      ()
  in
  let a = make () and b = make () in
  check (Input.history a = [ Console_text.sanitize ~keep_newlines:false s ]);
  let echo, lines = Input.feed a s in
  let echoes = Buffer.create 64 and completed = ref [] in
  String.iter
    (fun c ->
      let e, l = Input.feed b (String.make 1 c) in
      Buffer.add_string echoes e;
      completed := !completed @ l)
    s;
  check_eq ~pp:Fmt.Dump.string echo (Buffer.contents echoes);
  check (lines = !completed);
  check (Input.pending a = Input.pending b && Input.history a = Input.history b);
  check (not (String.contains echo '\027'));
  let masked = Input.v ~mask:"*" () in
  ignore (Input.feed masked s);
  ignore (Input.feed masked "\n");
  check (Input.history masked = [])

let suite = ("input", [ test_case "chunking and masking" [ bytes ] test_chunks ])
