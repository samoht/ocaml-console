(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* Feeding input at once or a byte at a time echoes and submits the same; a
   masked editor keeps no history; the echo never holds an escape the input
   did not ask for. *)
let test_chunks s =
  let make () =
    Console_input.v ~prompt:s ~history:[ s ]
      ~complete:(fun word -> [ word; word ^ s ])
      ()
  in
  let a = make () and b = make () in
  let strings = List.equal String.equal in
  check
    (strings (Console_input.history a)
       [ Console_text.sanitize ~keep_newlines:false s ]);
  let echo, lines = Console_input.feed a s in
  let echoes = Buffer.create 64 and completed = ref [] in
  String.iter
    (fun c ->
      let e, l = Console_input.feed b (String.make 1 c) in
      Buffer.add_string echoes e;
      completed := !completed @ l)
    s;
  check_eq ~pp:Fmt.Dump.string echo (Buffer.contents echoes);
  check (strings lines !completed);
  check
    (String.equal (Console_input.pending a) (Console_input.pending b)
    && strings (Console_input.history a) (Console_input.history b));
  check (not (String.contains echo '\027'));
  let masked = Console_input.v ~mask:"*" () in
  ignore (Console_input.feed masked s);
  ignore (Console_input.feed masked "\n");
  check (Console_input.history masked = [])

(* The keys that edit a line in place: characters one or two cells wide,
   both erasing keys and the arrows that move along the line. *)
let keys = [| "a"; "b"; "Z"; "\u{754c}"; "\127"; "\b"; "\027[D"; "\027[C" |]
let key = map [ uint8 ] (fun k -> keys.(k mod Array.length keys))

(* The echo of any editing, written on a terminal, shows the line being
   typed. *)
let test_echo_shows_the_line keys =
  let editor = Console_input.v () in
  let term = Console_vte.v ~raw:true ~rows:1 ~cols:200 () in
  List.iter
    (fun k -> Console_vte.feed term (fst (Console_input.feed editor k)))
    keys;
  let shown = match Console_vte.screen term with [] -> "" | row :: _ -> row in
  check_eq ~pp:Fmt.Dump.string (Console_input.pending editor) shown

let suite =
  ( "input",
    [
      test_case "chunking and masking" [ bytes ] test_chunks;
      test_case "the echo shows the line" [ list key ] test_echo_shows_the_line;
    ] )
