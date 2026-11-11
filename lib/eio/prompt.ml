(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A prompt reads its answers from an Eio flow, so waiting for an answer
   suspends the fiber, not the whole domain. It writes its questions on a
   formatter, like every other output of the library. *)

type t = {
  interactive : bool;
  terminal : Eio_unix.Fd.t option;
  input : Eio.Buf_read.t;
  ppf : Format.formatter;
}

(* The longest answer, in bytes. Without this cap, a source that never sends a
   line feed would grow the buffer without bound. A longer line is an [Error].
   The buffer holds an answer and its CRLF. *)
let max_answer = 4096

(* The input is a terminal if its descriptor is one. A flow with no
   descriptor, such as a string source or a mock's pipe, is not a terminal. *)
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
    input = Eio.Buf_read.of_flow ~max_size:(max_answer + 2) stdin;
    ppf;
  }

type error = [ `End_of_input | `Msg of string ]

let pp_error ppf = function
  | `End_of_input -> Fmt.string ppf "end of input"
  | `Msg m -> Fmt.string ppf m

let interactive t = t.interactive

let line t question =
  if not t.interactive then Error (`Msg "no terminal to ask on")
  else
    Backend.with_terminal @@ fun () ->
    (* The question goes out first, so it is on the terminal even when the
       answer never comes. *)
    Fmt.pf t.ppf "%s%!" (Console_text.sanitize question);
    (* [Buf_read.line] returns an unterminated last line as a line. Here, an
       answer cut short by the end of the input is [`End_of_input]. *)
    let read () =
      let line = Eio.Buf_read.take_while (fun c -> c <> '\n') t.input in
      let answer =
        if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1)
        else line
      in
      if String.length answer > max_answer then None
      else begin
        Eio.Buf_read.char '\n' t.input;
        Some answer
      end
    in
    (* Skip the rest of the line, so that the next question reads the next
       line. Skipping, unlike reading, keeps the buffer within its cap. *)
    let too_long () =
      Eio.Buf_read.skip_while (fun c -> c <> '\n') t.input;
      (try Eio.Buf_read.char '\n' t.input with End_of_file -> ());
      Error (`Msg "no answer: the line is too long")
    in
    match read () with
    | Some answer -> Ok answer
    | None | (exception Eio.Buf_read.Buffer_limit_exceeded) -> too_long ()
    | exception End_of_file -> Error `End_of_input

let confirm t question =
  match line t (question ^ " [y/N] ") with
  | Error _ as e -> e
  | Ok answer -> (
      match String.lowercase_ascii (String.trim answer) with
      | "y" | "yes" -> Ok true
      | _ -> Ok false)

(* The terminal's line discipline does the echo. To mask what is typed, we
   switch echo off on the descriptor behind the input. Eio has no termios, so
   this part uses Unix. Reads and writes still use the prompt's flows. *)
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

let rec skip_line t =
  match Eio.Buf_read.any_char t.input with
  | exception End_of_file -> Error `End_of_input
  | '\r' | '\n' ->
      Fmt.pf t.ppf "@.";
      Error (`Msg "no answer: the line is too long")
  | _ -> skip_line t

let rec read_masked t input n =
  if n > max_answer then skip_line t
  else
    match Eio.Buf_read.any_char t.input with
    | exception End_of_file -> Error `End_of_input
    | c -> (
        let echo, lines = Console_input.feed input (String.make 1 c) in
        Fmt.pf t.ppf "%s%!" echo;
        match lines with
        | line :: _ -> Ok line
        | [] -> read_masked t input (n + 1))

let secret t question =
  match t.terminal with
  | None -> Error (`Msg "no terminal to mask the answer on")
  | Some _ when not t.interactive -> Error (`Msg "no terminal to ask on")
  | Some fd -> (
      try
        Backend.with_terminal @@ fun () ->
        Eio_unix.Fd.use_exn "secret" fd @@ fun fd ->
        let original = Unix.tcgetattr fd in
        let restore () = Unix.tcsetattr fd Unix.TCSANOW original in
        Fmt.pf t.ppf "%s%!" (Console_text.sanitize question);
        Fun.protect ~finally:restore @@ fun () ->
        Console_driver.on_signals ~restore @@ fun () ->
        (* [TCSAFLUSH] discards the unread input, as getpass(3) does, because
           the terminal already echoed what was typed before echo went off. *)
        Unix.tcsetattr fd Unix.TCSAFLUSH (masked_attributes original);
        (* [read_masked] returns an error after [max_answer] bytes. The
           editor's own cap is above that, so the editor never refuses a byte
           first. *)
        let input = Console_input.v ~mask:"*" ~max_line:(max_answer + 1) () in
        read_masked t input 0
      with Unix.Unix_error (e, fn, _) ->
        Fmt.error_msg "%s: %s" fn (Unix.error_message e))

(* In keys mode, the terminal sends keys as they are typed, without echo.
   While SIGTSTP stops the program, [suspend] restores the original terminal
   mode. It sets keys mode again when the program continues. *)

let keys t f =
  match t.terminal with
  | None -> Error (`Msg "no terminal to read keys on")
  | Some _ when not t.interactive -> Error (`Msg "no terminal to ask on")
  | Some fd -> (
      try
        Eio_unix.Fd.use_exn "keys" fd @@ fun fd ->
        let original = Unix.tcgetattr fd in
        let raw () =
          Unix.tcsetattr fd Unix.TCSANOW (masked_attributes original)
        in
        let restore () = Unix.tcsetattr fd Unix.TCSANOW original in
        let suspend stop =
          restore ();
          Fun.protect stop ~finally:raw
        in
        Fun.protect ~finally:restore @@ fun () ->
        Console_driver.on_signals ~suspend ~restore @@ fun () ->
        raw ();
        Ok (f t.input)
      with
      | End_of_file -> Error `End_of_input
      | Unix.Unix_error (e, fn, _) ->
          Fmt.error_msg "%s: %s" fn (Unix.error_message e))
