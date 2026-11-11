(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* Feeding input at once or a byte at a time gives the same echo, lines,
   pending text and history. The editor keeps the given history sanitized.
   The echo has no escape character. A masked editor keeps no history. *)
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

(* Keys that edit a line in place: characters one or two cells wide, both
   erase keys, and the left and right arrows. A combining mark, a spacing
   mark, a VS16 and a ZWJ join the character before them. A VS16 after the
   heart, or a ZWJ between two emoji, makes an emoji sequence two cells
   wide. *)
let keys =
  [|
    "a";
    "b";
    "Z";
    "\u{754c}";
    "\u{0301}";
    "\u{093F}";
    "\u{2764}";
    "\u{fe0f}";
    "\u{200d}";
    "\u{1f469}";
    "\127";
    "\b";
    "\027[D";
    "\027[C";
  |]

let key = map [ uint8 ] (fun k -> keys.(k mod Array.length keys))

(* After any edit, the echo written on a terminal shows the pending line. *)
let test_echo_shows_the_line keys =
  let editor = Console_input.v () in
  let term = Console_vte.v ~raw:true ~rows:1 ~cols:200 () in
  List.iter
    (fun k ->
      Console_vte.feed term (fst (Console_input.feed editor k));
      guard (Console_vte.counts_as_console (Console_input.pending editor)))
    keys;
  let shown = match Console_vte.screen term with [] -> "" | row :: _ -> row in
  check_eq ~pp:Fmt.Dump.string (Console_input.pending editor) shown

(* After any keys, the cursor is between two clusters of the line, never
   inside one. This holds even after a character that joins the cluster
   after the cursor. The test types [|] and checks that it splits the line
   at a cluster boundary. *)
let test_cursor_between_clusters keys =
  let editor = Console_input.v () in
  List.iter (fun k -> ignore (Console_input.feed editor k)) keys;
  ignore (Console_input.feed editor "|");
  match String.split_on_char '|' (Console_input.pending editor) with
  | [ before; after ] ->
      check_eq
        ~pp:Fmt.(Dump.list Dump.string)
        (Console_width.clusters (before ^ after))
        (Console_width.clusters before @ Console_width.clusters after)
  | _ -> failf "not one | in %S" (Console_input.pending editor)

(* Inserting a multi-codepoint cluster before a suffix redraws the suffix at
   its new column. This holds even when the cluster changes width as it is
   typed. *)
let test_insert_cluster k n split =
  let clusters =
    [|
      "\u{2764}\u{fe0f}";
      "\u{1f469}\u{200d}\u{1f4bb}";
      "\u{1f44d}\u{1f3fd}";
      "\u{1f1eb}\u{1f1f7}";
      "e\u{0301}";
      "a\u{093f}";
    |]
  in
  let cluster = clusters.(k mod Array.length clusters) in
  let suffix = String.make (1 + (n mod 8)) 'x' in
  let editor = Console_input.v () in
  let actual = Console_vte.v ~raw:true ~rows:1 ~cols:80 ()
  and expected = Console_vte.v ~raw:true ~rows:1 ~cols:80 () in
  let input =
    suffix
    ^ String.concat "" (List.init (String.length suffix) (fun _ -> "\027[D"))
    ^ cluster
  in
  let feed s =
    let echo, lines = Console_input.feed editor s in
    check (lines = []);
    check (not (String.contains echo '\027'));
    Console_vte.feed actual echo
  in
  if split then String.iter (fun c -> feed (String.make 1 c)) input
  else feed input;
  check_eq ~pp:Fmt.Dump.string (cluster ^ suffix) (Console_input.pending editor);
  Console_vte.feed expected cluster;
  let cursor = Console_vte.cursor_pos expected in
  Console_vte.feed expected suffix;
  check_eq ~pp:Fmt.Dump.string
    (Console_vte.to_string expected)
    (Console_vte.to_string actual);
  check (cursor = Console_vte.cursor_pos actual)

(* A password mask shows one mask per cluster, not per codepoint. This
   holds also when a multibyte character arrives one byte at a time. *)
let test_masked_clusters data split =
  let clusters =
    [|
      "a";
      "e\u{0301}";
      "\u{754c}";
      "\u{2764}\u{fe0f}";
      "\u{1f469}\u{200d}\u{1f4bb}";
      "\u{1f44d}\u{1f3fd}";
      "\u{1f1eb}\u{1f1f7}";
      "a\u{093f}";
    |]
  in
  let count = min 32 (String.length data) in
  let secret =
    String.concat ""
      (List.init count (fun i ->
           clusters.(Char.code data.[i] mod Array.length clusters)))
  in
  let editor = Console_input.v ~mask:"*" () in
  let echo = Buffer.create 64 in
  let feed input =
    let out, lines = Console_input.feed editor input in
    check (lines = []);
    Buffer.add_string echo out
  in
  if split then String.iter (fun c -> feed (String.make 1 c)) secret
  else feed secret;
  check_eq ~pp:Fmt.Dump.string (String.make count '*') (Buffer.contents echo);
  check_eq ~pp:Fmt.Dump.string secret (Console_input.pending editor);
  let out, lines = Console_input.feed editor "\r" in
  check_eq ~pp:Fmt.Dump.string "\r\n" out;
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    ~eq:(List.equal String.equal) [ secret ] lines;
  check (Console_input.history editor = [])

(* A masked editor submits the exact bytes typed, including malformed and
   truncated UTF-8. Feeding at once or a byte at a time gives the same
   secret and the same echo. *)
let test_secret_bytes data =
  let data = String.sub data 0 (min 256 (String.length data)) in
  let secret =
    "\255"
    ^ String.map
        (fun c -> if Char.code c < 32 || c = '\127' then 'x' else c)
        data
    ^ "\195"
  in
  let whole = Console_input.v ~mask:"*" ()
  and pieces = Console_input.v ~mask:"*" () in
  let echo, lines = Console_input.feed whole (secret ^ "\r\n") in
  let output = Buffer.create 64 and submitted = ref [] in
  String.iter
    (fun c ->
      let out, lines = Console_input.feed pieces (String.make 1 c) in
      Buffer.add_string output out;
      submitted := List.rev_append lines !submitted)
    (secret ^ "\r\n");
  check_eq ~pp:Fmt.Dump.string echo (Buffer.contents output);
  List.iter
    (fun actual ->
      check_eq
        ~pp:Fmt.(Dump.list Dump.string)
        ~eq:(List.equal String.equal) [ secret ] actual)
    [ lines; List.rev !submitted ];
  List.iter
    (fun editor ->
      check_eq ~pp:Fmt.Dump.string "" (Console_input.pending editor);
      check (Console_input.history editor = []))
    [ whole; pieces ]

(* A masked editor keeps no history, not even the history it is given. *)
let test_masked_history_seed data =
  let editor =
    Console_input.v ~mask:"*" ~history:[ "previous answer"; data ] ()
  in
  ignore (Console_input.feed editor "new answer\r");
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    ~eq:(List.equal String.equal) []
    (Console_input.history editor)

let suite =
  ( "input",
    [
      test_case "masked editors retain no seeded history" [ bytes ]
        test_masked_history_seed;
      test_case "masked arbitrary bytes survive every chunk boundary" [ bytes ]
        test_secret_bytes;
      test_case "passwords echo one mask per grapheme cluster" [ bytes; bool ]
        test_masked_clusters;
      test_case "inserting a cluster redraws its suffix" [ uint8; uint8; bool ]
        test_insert_cluster;
      test_case "chunking and masking" [ bytes ] test_chunks;
      test_case "the echo shows the line" [ list key ] test_echo_shows_the_line;
      test_case "the cursor stands between clusters"
        [ list key ]
        test_cursor_between_clusters;
    ] )
