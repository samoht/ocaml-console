(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A prompt reads its answers from an Eio flow, so that waiting for one
   suspends the fiber instead of parking the domain, and writes its questions
   on a formatter, as every other output of the library is written. *)

type t = {
  interactive : bool;
  terminal : Eio_unix.Fd.t option;
  input : Eio.Buf_read.t;
  ppf : Format.formatter;
}

(* A typed answer is one line from a human. The cap keeps a source that never
   sends a terminator from growing the buffer without bound; crossing it is an
   [Error] like any other unanswerable question. *)
let max_answer = 4096

(* The one genuine Unix boundary: whether there is a terminal on the other end
   of the input is a property of its descriptor, and a flow with no descriptor
   -- a string source, a pipe from a mock -- has nobody to ask. *)
let terminal stdin =
  Option.bind (Eio_unix.Resource.fd_opt stdin) (fun fd ->
      if Eio_unix.Fd.use_exn "isatty" fd Unix.isatty then Some fd else None)

let v ?interactive ?(ppf = Fmt.stderr) stdin =
  let terminal = terminal stdin in
  let interactive =
    Option.value interactive ~default:(Option.is_some terminal)
  in
  {
    interactive;
    terminal;
    input = Eio.Buf_read.of_flow ~max_size:max_answer stdin;
    ppf;
  }

let interactive t = t.interactive

let line t question =
  if not t.interactive then Error (`Msg "no terminal to ask on")
  else
    Session.with_terminal @@ fun () ->
    (* The question goes out first, so it is on the terminal even when the
       answer never comes. *)
    Fmt.pf t.ppf "%s%!" (Console_text.sanitize question);
    (* [Buf_read.line] returns an unterminated last line as if it were one;
       an answer cut short by the end of the input is no answer. *)
    let read () =
      let line = Eio.Buf_read.take_while (fun c -> c <> '\n') t.input in
      Eio.Buf_read.char '\n' t.input;
      let n = String.length line in
      if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
    in
    match read () with
    | answer -> Ok answer
    | exception End_of_file -> Error (`Msg "no answer: the input ended")
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        Error (`Msg "no answer: the line is too long")

let confirm t question =
  match line t (question ^ " [y/N] ") with
  | Error (`Msg _ as e) -> Error e
  | Ok answer -> (
      match String.lowercase_ascii (String.trim answer) with
      | "y" | "yes" -> Ok true
      | _ -> Ok false)

(* Echo is the terminal's own line discipline, so masking what is typed means
   switching it off on the descriptor behind the input. Eio has no termios, so
   that part is Unix; reading and writing stay on the prompt's flows. *)
let masked_attributes attr =
  {
    attr with
    Unix.c_icanon = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;
    c_vmin = 1;
    c_vtime = 0;
  }

let rec read_masked t input =
  match Eio.Buf_read.any_char t.input with
  | exception End_of_file -> Error (`Msg "no answer: the input ended")
  | c -> (
      let echo, lines = Console.Input.feed input (String.make 1 c) in
      Fmt.pf t.ppf "%s%!" echo;
      match lines with line :: _ -> Ok line | [] -> read_masked t input)

let secret t question =
  match t.terminal with
  | None -> Error (`Msg "no terminal to mask the answer on")
  | Some _ when not t.interactive -> Error (`Msg "no terminal to ask on")
  | Some fd -> (
      try
        Session.with_terminal @@ fun () ->
        Eio_unix.Fd.use_exn "secret" fd @@ fun fd ->
        let original = Unix.tcgetattr fd in
        let restore () = Unix.tcsetattr fd Unix.TCSANOW original in
        Fmt.pf t.ppf "%s%!" (Console_text.sanitize question);
        Fun.protect ~finally:restore @@ fun () ->
        Interrupt.protect ~restore @@ fun () ->
        Unix.tcsetattr fd Unix.TCSANOW (masked_attributes original);
        read_masked t (Console.Input.v ~mask:"*" ())
      with Unix.Unix_error (e, fn, _) ->
        Fmt.error_msg "%s: %s" fn (Unix.error_message e))
