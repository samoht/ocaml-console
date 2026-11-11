(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Console_eio.Prompt

let run f = Eio_main.run (fun _env -> f ())

(* A prompt over a string source and a buffer. It takes the same code path as a
   terminal, with scripted answers and captured questions. *)
let prompt ?interactive input =
  let out = Buffer.create 64 in
  let t =
    Prompt.v ?interactive
      ~ppf:(Format.formatter_of_buffer out)
      (Eio.Flow.string_source input)
  in
  (t, out)

(* The implementation chooses the wording of a refusal. So the tests compare
   answers after mapping every error except the end of the input to [Refused].
   One message has specified content: the gate does not echo the word it asks
   for. It has its own assertion. *)
type 'a answer = Got of 'a | Ended | Refused

let seen = function
  | Ok v -> Got v
  | Error `End_of_input -> Ended
  | Error (`Msg _) -> Refused

let answer t =
  let equal a b =
    match (a, b) with
    | Got x, Got y -> Alcotest.equal t x y
    | Ended, Ended | Refused, Refused -> true
    | (Got _ | Ended | Refused), _ -> false
  in
  let pp ppf = function
    | Got v -> Fmt.pf ppf "Got %a" (Alcotest.pp t) v
    | Ended -> Fmt.string ppf "Ended"
    | Refused -> Fmt.string ppf "Refused"
  in
  Alcotest.testable pp equal

let string_answer = answer Alcotest.string
let bool_answer = answer Alcotest.bool

(* A string source is not a terminal. So the default prompt refuses every
   question and writes nothing. *)
let test_a_string_source_is_not_a_terminal () =
  run @@ fun () ->
  let t, out = prompt "y\n" in
  Alcotest.(check bool) "not interactive" false (Prompt.interactive t);
  Alcotest.check string_answer "line" Refused (seen (Prompt.line t "name: "));
  Alcotest.check bool_answer "confirm" Refused
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "asked nothing" "" (Buffer.contents out)

(* One [t] owns the read buffer, so the second question gets the second line,
   not what was left from the first read. *)
let test_line_asks_verbatim_and_keeps_its_place () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "alpha\nbeta\n" in
  Alcotest.check string_answer "first answer" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.(check string) "first question" "name: " (Buffer.contents out);
  Alcotest.check string_answer "second answer" (Got "beta")
    (seen (Prompt.line t "again: "));
  Alcotest.(check string) "both questions" "name: again: " (Buffer.contents out)

(* The answer is the typed text, spaces included, without the terminator. *)
let test_line_keeps_the_spaces_it_was_given () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "  spaced out  \n" in
  Alcotest.check string_answer "answer" (Got "  spaced out  ")
    (seen (Prompt.line t "name: "))

let test_line_drops_a_crlf_terminator () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "alpha\r\nbeta\r\n" in
  Alcotest.check string_answer "first" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.check string_answer "second" (Got "beta")
    (seen (Prompt.line t "again: "))

(* The question is written before the read, so it is on the terminal even when
   no answer comes. *)
let test_line_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "" in
  Alcotest.check string_answer "answer" Ended (seen (Prompt.line t "name: "));
  Alcotest.(check string) "question" "name: " (Buffer.contents out);
  let t, _ = prompt ~interactive:true "cut" in
  Alcotest.check string_answer "a line cut short" Ended
    (seen (Prompt.line t "name: "))

let test_confirm_asks_with_a_default_of_no () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "y\n" in
  Alcotest.check bool_answer "answer" (Got true)
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "question" "delete? [y/N] " (Buffer.contents out)

let check_confirm input expected =
  let t, _ = prompt ~interactive:true (input ^ "\n") in
  Alcotest.check bool_answer
    (Fmt.str "confirm %S" input)
    (Got expected)
    (seen (Prompt.confirm t "delete?"))

let test_confirm_takes_y_and_yes_in_any_case () =
  run @@ fun () ->
  List.iter
    (fun input -> check_confirm input true)
    [ "y"; "Y"; "yes"; "  YES  " ]

let test_confirm_takes_everything_else_as_no () =
  run @@ fun () ->
  List.iter (fun input -> check_confirm input false) [ "n"; ""; "maybe" ]

let test_confirm_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "" in
  Alcotest.check bool_answer "answer" Ended (seen (Prompt.confirm t "delete?"))

(* A question is text: an escape code in it cannot clear the screen. *)
let test_question_is_text () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "y\n" in
  ignore (Prompt.confirm t "\027[2Jdelete?");
  Alcotest.(check bool)
    "no escape" false
    (String.contains (Buffer.contents out) '\027')

let test_secret_without_a_terminal_is_refused () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "abc\n" in
  Alcotest.check string_answer "answer" Refused
    (seen (Prompt.secret t "secret: "));
  Alcotest.(check string) "nothing asked" "" (Buffer.contents out)

(* [with_stdin ~stdin f] is [f ()] with standard input on [stdin]. Standard
   input is restored when [f] ends. *)
let with_stdin ~stdin f =
  let saved = Unix.dup Unix.stdin in
  Fun.protect ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      Unix.close saved)
  @@ fun () ->
  Unix.dup2 stdin Unix.stdin;
  f ()

(* [with_pty f] is [f env tty master] on a new pseudo-terminal. [tty] and
   [master] each have their own descriptor. *)
let with_pty f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Pty_harness.open_pty ~sw in
  let tty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup in
  let master = Eio_unix.Fd.use_exn "pty" (Eio_unix.Pty.pty pty) Unix.dup in
  Fun.protect ~finally:(fun () -> List.iter Unix.close [ tty; master ])
  @@ fun () -> f env tty master

let masked tty = not (Unix.tcgetattr tty).Unix.c_echo

(* Whether the terminal has output for its reader, without waiting. *)
let ready fd =
  match Unix.select [ fd ] [] [] 0. with [], _, _ -> false | _ -> true

(* [drain master] reads what the terminal shows until it has nothing more to
   show for now. *)
let rec drain master =
  if ready master then begin
    ignore (Unix.read master (Bytes.create 4096) 0 4096 : int);
    drain master
  end

let type_in master s =
  ignore (Unix.write_substring master s 0 (String.length s) : int)

(* [await_echo master s] waits until the terminal shows [s]. On Linux, typed
   input reaches the line discipline later, and a flush before that misses it.
   Once the input is echoed, it is there. *)
let await_echo master s =
  let shown = Buffer.create 64 in
  let buf = Bytes.create 4096 in
  while not (Display_harness.contains s (Buffer.contents shown)) do
    ignore (Unix.select [ master ] [] [] (-1.));
    let n = Unix.read master buf 0 (Bytes.length buf) in
    Buffer.add_subbytes shown buf 0 n
  done

(* [prompt_on env ~echoed] is a prompt on standard input. Its questions and
   masks go to a formatter that counts the masks, [*], in [echoed]. *)
let prompt_on env ~echoed =
  let ppf =
    Format.make_formatter
      (fun s off len ->
        for i = off to off + len - 1 do
          if s.[i] = '*' then Atomic.incr echoed
        done)
      ignore
  in
  Prompt.v ~ppf (Eio.Stdenv.stdin env)

(* [secret_typed typed] is the answer of [secret] on a pseudo-terminal, where
   another fiber of the domain types [typed]. That fiber runs only if the read
   lets it: a read that blocks the domain hangs, and the run's guard ends it.
   The fiber types once the question is masked, because input typed before is
   discarded. *)
let secret_typed typed =
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      let answer, () =
        Eio.Fiber.pair
          (fun () -> Prompt.secret t "secret: ")
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            type_in master typed)
      in
      seen answer)

(* [secret] waits for the terminal as a fiber waits, so the rest of the domain
   runs in the meantime. *)
let test_secret_leaves_the_domain_running () =
  Alcotest.check string_answer "the typed line" (Got "abc")
    (secret_typed "abc\n")

(* A password is the typed bytes. A bidi mark, a C1 control and a byte that is
   not UTF-8 come back as typed. *)
let test_secret_keeps_bytes () =
  let password = "\u{202e}p\xc2\x85w\xff" in
  Alcotest.check string_answer "the bytes typed" (Got password)
    (secret_typed (password ^ "\n"))

(* [secret] does not take a line longer than its cap. Another domain types the
   line in pieces once the question is masked. It types each piece after the
   prompt has masked everything typed before, so the terminal's input queue
   never fills. Past the cap, the prompt masks no more, and the rest of the
   line, which is shorter than a queue, is typed at once. *)
let test_secret_cap () =
  let line = String.make 5000 'a' ^ "\n" in
  let cap = 4097 in
  let result =
    with_pty @@ fun env tty master ->
    let echoed = Atomic.make 0 in
    let typist =
      Domain.spawn (fun () ->
          while not (masked tty) do
            Domain.cpu_relax ()
          done;
          let rec go i =
            if i < cap then begin
              let n = min 64 (cap - i) in
              ignore (Unix.write_substring master line i n : int);
              while Atomic.get echoed < i + n do
                Domain.cpu_relax ()
              done;
              go (i + n)
            end
            else
              ignore
                (Unix.write_substring master line i (String.length line - i)
                  : int)
          in
          go 0)
    in
    let result =
      with_stdin ~stdin:tty (fun () ->
          Prompt.secret (prompt_on env ~echoed) "secret: ")
    in
    Domain.join typist;
    seen result
  in
  match result with
  | Got _ -> Alcotest.fail "a line of 5000 bytes taken"
  | _ -> ()

(* Text typed before [secret] masks the terminal is echoed, as under getpass(3),
   but it is not the answer. [secret] discards it and reads what is typed after
   the question is masked. [typed] is typed first. If [before] is not empty, a
   {!Prompt.line} answered with [before] is asked first. A domain plays the
   terminal. It reads what the terminal shows, because masking waits until that
   output is read. Once echo is off and the input discarded, it types the
   answer. *)
let secret_after ~before typed =
  let result =
    with_pty @@ fun env tty master ->
    type_in master typed;
    await_echo master "early";
    let typist =
      Domain.spawn (fun () ->
          while not (masked tty) do
            drain master;
            Domain.cpu_relax ()
          done;
          type_in master "late\n")
    in
    let result =
      with_stdin ~stdin:tty (fun () ->
          let t = prompt_on env ~echoed:(Atomic.make 0) in
          if before <> "" then
            Alcotest.check string_answer "the line" (Got before)
              (seen (Prompt.line t "line: "));
          Prompt.secret t "secret: ")
    in
    Domain.join typist;
    result
  in
  Alcotest.check string_answer "the line typed once masked" (Got "late")
    (seen result)

let test_secret_drops_typeahead () = secret_after ~before:"" "early\n"

(* A line typed ahead of a {!Prompt.line} question is not kept for the next
   [secret]. The terminal gives [line] its own line only, and [secret] discards
   the rest, which the terminal echoed. *)
let test_secret_after_a_line () = secret_after ~before:"name" "name\nearly\n"

(* While one fiber waits for an answer, the output of another fiber still
   belongs to its task. Only the prompting fiber writes directly. [run] takes
   the program's output from [Fmt.stdout], which writes into a buffer here. *)
let test_prompt_leaves_other_fibers_attributed () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let source, sink = Eio_unix.pipe sw in
  let questions = Buffer.create 64 in
  let t =
    Prompt.v ~interactive:true
      ~ppf:(Format.formatter_of_buffer questions)
      source
  in
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    { saved with out_string = Buffer.add_substring out; out_flush = ignore };
  let answer =
    Fun.protect
      ~finally:(fun () ->
        Format.pp_set_formatter_out_functions Fmt.stdout saved)
      (fun () ->
        Console_eio.run ~clock ~ppf:Fmt.stdout ~progress:`Json (fun display ->
            Eio.Fiber.pair
              (fun () -> Prompt.line t "name: ")
              (fun () ->
                Console.Task.run display "work" (fun _ -> Fmt.pr "from work@.");
                Eio.Flow.copy_string "alice\n" sink)))
  in
  Alcotest.check string_answer "the answer" (Got "alice") (seen (fst answer));
  Alcotest.(check bool)
    "the line is the task's output" true
    (Display_harness.contains
       {|"event":"output","id":0,"task":["work"],"text":"from work"|}
       (Buffer.contents out))

(* [keys] reads each key as it is typed, without echo, and restores the terminal
   as it was. Without a terminal, it is refused. *)
let test_keys () =
  run (fun () ->
      let t, _ = prompt ~interactive:true "abc" in
      Alcotest.(check bool)
        "refused without a terminal" true
        (Result.is_error (Prompt.keys t (fun _ -> ()))));
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      let before = Unix.tcgetattr tty in
      let keys, () =
        Eio.Fiber.pair
          (fun () ->
            Prompt.keys t (fun input ->
                let a = Eio.Buf_read.any_char input in
                let b = Eio.Buf_read.any_char input in
                String.init 2 (function 0 -> a | _ -> b)))
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            Alcotest.(check bool)
              "key by key" false (Unix.tcgetattr tty).Unix.c_icanon;
            type_in master "q ")
      in
      (match keys with
      | Ok k -> Alcotest.(check string) "the keys" "q " k
      | Error _ -> Alcotest.fail "refused on a terminal");
      Alcotest.(check bool) "given back" true (Unix.tcgetattr tty = before))

let test_keys_at_end_of_input () =
  with_pty @@ fun env tty _ ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      Alcotest.check string_answer "the end of the input" Ended
        (seen (Prompt.keys t (fun _ -> raise End_of_file))))

let test_keys_split_sequences () =
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let prompt = prompt_on env ~echoed:(Atomic.make 0) in
      let before = Unix.tcgetattr tty in
      let typed = "界\027[A👩‍💻\027OD" in
      let received, () =
        Eio.Fiber.pair
          (fun () ->
            Prompt.keys prompt (fun input ->
                let out = Buffer.create (String.length typed) in
                for _ = 1 to String.length typed do
                  Buffer.add_char out (Eio.Buf_read.any_char input)
                done;
                Buffer.contents out))
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            String.iter
              (fun byte ->
                type_in master (String.make 1 byte);
                Eio.Fiber.yield ())
              typed)
      in
      (match received with
      | Ok keys -> Alcotest.(check string) "every key byte, in order" typed keys
      | Error error -> Alcotest.failf "%a" Prompt.pp_error error);
      Alcotest.(check bool)
        "terminal attributes restored" true
        (Unix.tcgetattr tty = before))

let test_pp_error () =
  Alcotest.(check string)
    "end of input" "end of input"
    (Fmt.str "%a" Prompt.pp_error `End_of_input);
  Alcotest.(check string)
    "a message" "no terminal"
    (Fmt.str "%a" Prompt.pp_error (`Msg "no terminal"))

let test_line_at_limit () =
  run @@ fun () ->
  let line = String.make 4096 'x' in
  let t, _ = prompt ~interactive:true (line ^ "\nnext\n") in
  let first = seen (Prompt.line t "") in
  Alcotest.check string_answer "the next answer stays intact" (Got "next")
    (seen (Prompt.line t ""));
  Alcotest.check string_answer "4096 bytes is not longer than 4096" (Got line)
    first

let suite =
  ( "prompt",
    [
      Alcotest.test_case "line accepts exactly 4096 answer bytes" `Quick
        test_line_at_limit;
      Alcotest.test_case "a string source is not a terminal" `Quick
        test_a_string_source_is_not_a_terminal;
      Alcotest.test_case "line asks verbatim and keeps its place" `Quick
        test_line_asks_verbatim_and_keeps_its_place;
      Alcotest.test_case "line keeps the spaces it was given" `Quick
        test_line_keeps_the_spaces_it_was_given;
      Alcotest.test_case "line drops a crlf terminator" `Quick
        test_line_drops_a_crlf_terminator;
      Alcotest.test_case "line at end of input is refused" `Quick
        test_line_at_end_of_input_is_refused;
      Alcotest.test_case "confirm asks with a default of no" `Quick
        test_confirm_asks_with_a_default_of_no;
      Alcotest.test_case "confirm takes y and yes in any case" `Quick
        test_confirm_takes_y_and_yes_in_any_case;
      Alcotest.test_case "confirm takes everything else as no" `Quick
        test_confirm_takes_everything_else_as_no;
      Alcotest.test_case "confirm at end of input is refused" `Quick
        test_confirm_at_end_of_input_is_refused;
      Alcotest.test_case "a question is text" `Quick test_question_is_text;
      Alcotest.test_case "secret without a terminal is refused" `Quick
        test_secret_without_a_terminal_is_refused;
      Alcotest.test_case "secret leaves the domain running" `Quick
        test_secret_leaves_the_domain_running;
      Alcotest.test_case "secret drops what was typed before it masks" `Quick
        test_secret_drops_typeahead;
      Alcotest.test_case "secret drops what was typed after a line" `Quick
        test_secret_after_a_line;
      Alcotest.test_case "a password is the bytes typed" `Quick
        test_secret_keeps_bytes;
      Alcotest.test_case "secret refuses a line of more than 4096 bytes" `Quick
        test_secret_cap;
      Alcotest.test_case "a prompt leaves other fibers' output to their task"
        `Quick test_prompt_leaves_other_fibers_attributed;
      Alcotest.test_case "keys" `Quick test_keys;
      Alcotest.test_case "keys at the end of the input" `Quick
        test_keys_at_end_of_input;
      Alcotest.test_case "keys preserve split UTF-8 and escape sequences" `Quick
        test_keys_split_sequences;
      Alcotest.test_case "an error formats" `Quick test_pp_error;
    ] )
