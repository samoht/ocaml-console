(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Console_eio.Prompt

let run f = Eio_main.run (fun _env -> f ())

(* A prompt over a string source and a buffer: the same code path a terminal
   takes, with the answers scripted and the questions captured. *)
let prompt ?interactive input =
  let out = Buffer.create 64 in
  let t =
    Prompt.v ?interactive
      ~ppf:(Format.formatter_of_buffer out)
      (Eio.Flow.string_source input)
  in
  (t, out)

(* The wording of a refusal is the implementation's to choose, so an answer is
   compared after collapsing every error but the end of the input to
   [Refused]. The one message whose content is itself a specification -- the
   gate must not echo the word it is asking for -- gets its own assertion. *)
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

(* A string source is not a terminal, so the default is a prompt that refuses
   every question without writing a byte. *)
let test_a_string_source_is_not_a_terminal () =
  run @@ fun () ->
  let t, out = prompt "y\n" in
  Alcotest.(check bool) "not interactive" false (Prompt.interactive t);
  Alcotest.check string_answer "line" Refused (seen (Prompt.line t "name: "));
  Alcotest.check bool_answer "confirm" Refused
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "asked nothing" "" (Buffer.contents out)

(* One [t] owns the read buffer, so the second question gets the second line
   rather than whatever survived the first read. *)
let test_line_asks_verbatim_and_keeps_its_place () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "alpha\nbeta\n" in
  Alcotest.check string_answer "first answer" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.(check string) "first question" "name: " (Buffer.contents out);
  Alcotest.check string_answer "second answer" (Got "beta")
    (seen (Prompt.line t "again: "));
  Alcotest.(check string) "both questions" "name: again: " (Buffer.contents out)

(* The answer is what was typed, spaces and all, minus the terminator. *)
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

(* The question goes out before the read, so it is on the terminal even when
   the answer never comes. *)
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

(* A question is text: an escape in it cannot clear the screen. *)
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

(* [f ()] with standard input on [stdin], given back as it was found. *)
let with_stdin ~stdin f =
  let saved = Unix.dup Unix.stdin in
  Fun.protect ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      Unix.close saved)
  @@ fun () ->
  Unix.dup2 stdin Unix.stdin;
  f ()

(* [secret t] answered on a pseudo-terminal with [typed], by another fiber
   of the domain if the read lets it run, or else by a watchdog domain, so
   that a read that parks the domain fails rather than hangs. It is the
   answer and whether the fiber typed it. *)
let secret_typed typed =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  let tty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup in
  let master = Eio_unix.Fd.use_exn "pty" (Eio_unix.Pty.pty pty) Unix.dup in
  Fun.protect ~finally:(fun () -> List.iter Unix.close [ tty; master ])
  @@ fun () ->
  let typed_once = Atomic.make false in
  let type_answer () =
    if Atomic.compare_and_set typed_once false true then
      ignore (Unix.write_substring master typed 0 (String.length typed) : int)
  in
  let watchdog =
    Domain.spawn (fun () ->
        let deadline = Unix.gettimeofday () +. 10. in
        while
          (not (Atomic.get typed_once)) && Unix.gettimeofday () < deadline
        do
          Unix.sleepf 0.01
        done;
        type_answer ())
  in
  let by_fiber = ref false in
  let result = ref (Error (`Msg "unread")) in
  with_stdin ~stdin:tty (fun () ->
      Eio.Fiber.both
        (fun () ->
          let t =
            Prompt.v
              ~ppf:(Format.formatter_of_buffer (Buffer.create 16))
              (Eio.Stdenv.stdin env)
          in
          result := Prompt.secret t "secret: ")
        (fun () ->
          Eio.Fiber.yield ();
          if not (Atomic.get typed_once) then begin
            by_fiber := true;
            type_answer ()
          end));
  Domain.join watchdog;
  (seen !result, !by_fiber)

(* [secret] waits for the terminal the way a fiber waits, so the rest of the
   domain runs meanwhile: the answer is typed by another fiber of the same
   domain, which only runs if the read let it. *)
let test_secret_leaves_the_domain_running () =
  let answer, by_fiber = secret_typed "abc\n" in
  Alcotest.check string_answer "the typed line" (Got "abc") answer;
  Alcotest.(check bool)
    "the domain ran while the terminal was awaited" true by_fiber

(* A password is the bytes typed: a bidi mark, a C1 control and a byte
   that is no UTF-8 come back as they were typed. *)
let test_secret_keeps_bytes () =
  let password = "\u{202e}p\xc2\x85w\xff" in
  let answer, _ = secret_typed (password ^ "\n") in
  Alcotest.check string_answer "the bytes typed" (Got password) answer

(* What is typed before [secret] masks the terminal is echoed, as it is
   under getpass(3), but it is not the answer: [secret] discards it and
   reads what is typed once the question is masked. [typed] is typed first,
   and a {!Prompt.line} answered [before] is asked first unless [before] is
   empty. A domain plays the
   terminal: once the echo is off it types the answer until the call
   returns, and it reads what the terminal shows, as masking waits for that
   output to be read. It types nothing while the mask is set: on macOS a
   line typed as the input is discarded can leave the read waiting. *)
let secret_after ~before typed =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  let tty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup in
  let master = Eio_unix.Fd.use_exn "pty" (Eio_unix.Pty.pty pty) Unix.dup in
  Fun.protect ~finally:(fun () -> List.iter Unix.close [ tty; master ])
  @@ fun () ->
  ignore (Unix.write_substring master typed 0 (String.length typed) : int);
  let answered = Atomic.make false in
  let typist =
    Domain.spawn (fun () ->
        let deadline = Unix.gettimeofday () +. 10. in
        let masked = ref false in
        while (not (Atomic.get answered)) && Unix.gettimeofday () < deadline do
          if !masked then
            ignore (Unix.write_substring master "late\n" 0 5 : int)
          else masked := not (Unix.tcgetattr tty).Unix.c_echo;
          match Unix.select [ master ] [] [] 0.05 with
          | [], _, _ -> ()
          | _ -> ignore (Unix.read master (Bytes.create 256) 0 256 : int)
        done)
  in
  let result =
    with_stdin ~stdin:tty (fun () ->
        let t =
          Prompt.v
            ~ppf:(Format.formatter_of_buffer (Buffer.create 16))
            (Eio.Stdenv.stdin env)
        in
        Fun.protect
          ~finally:(fun () -> Atomic.set answered true)
          (fun () ->
            if before <> "" then
              Alcotest.check string_answer "the line" (Got before)
                (seen (Prompt.line t "line: "));
            Prompt.secret t "secret: "))
  in
  Domain.join typist;
  Alcotest.check string_answer "the line typed once masked" (Got "late")
    (seen result)

let test_secret_drops_typeahead () = secret_after ~before:"" "early\n"

(* A line typed ahead of a question that {!Prompt.line} answers is not
   kept for the [secret] after it: the terminal hands [line] its own line
   alone, and [secret] discards the rest, which the terminal echoed. *)
let test_secret_after_a_line () = secret_after ~before:"name" "name\nearly\n"

let suite =
  ( "prompt",
    [
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
    ] )
