(* [`Auto] draws a live region where one can be drawn, on the descriptor the
   display writes to: [Console_eio.run ~ppf:Fmt.stderr] asks standard error,
   whatever standard output is. A terminal whose [TERM] is [dumb] has no cursor
   to move, so it gets the appending display, the one a pipe gets. Each case
   puts a pseudo-terminal or a pipe on the two descriptors for its span and
   reads what the display wrote: a live region hides the cursor first
   (ESC [?25l), and an appending display writes no escape at all. *)

let hide_cursor = "\027[?25l"

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

let tty pty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup

(* [f ()] with standard output on [stdout], standard error on [stderr] and
   [TERM] set to [term], each given back as it was found. *)
let with_fds ~stdout ~stderr ~term f =
  let saved_out = Unix.dup Unix.stdout and saved_err = Unix.dup Unix.stderr in
  let saved_term = Sys.getenv_opt "TERM" in
  Fun.protect ~finally:(fun () ->
      flush Stdlib.stdout;
      flush Stdlib.stderr;
      Unix.dup2 saved_out Unix.stdout;
      Unix.dup2 saved_err Unix.stderr;
      Unix.close saved_out;
      Unix.close saved_err;
      Unix.putenv "TERM" (Option.value saved_term ~default:""))
  @@ fun () ->
  Unix.dup2 stdout Unix.stdout;
  Unix.dup2 stderr Unix.stderr;
  Unix.putenv "TERM" term;
  f ()

(* One task, drawn on standard error by [Console_eio.run] in [`Auto], on a
   clock that never moves: the case asserts which display was chosen and
   nothing about the machine's pace. *)
let draw_a_task () =
  let mock = Eio_mock.Clock.make () in
  let clock = (mock :> float Eio.Time.clock_ty Eio.Resource.t) in
  Console_eio.run ~clock ~ppf:Fmt.stderr ~mode:`Auto (fun display ->
      let task = Console.Display.task display "step" in
      Console.Display.succeed task)

(* A mark no display writes, so a reader of the terminal knows it has read
   everything written before it. *)
let end_mark = "<end of the case's output>"

let mark fd =
  ignore (Unix.write_substring fd end_mark 0 (String.length end_mark) : int)

let read_to_mark env pty =
  let written = Buffer.create 256 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let rec loop () =
    if not (String.ends_with ~suffix:end_mark (Buffer.contents written)) then begin
      let n = Eio.Flow.single_read source buf in
      Buffer.add_string written (Cstruct.to_string ~len:n buf);
      loop ()
    end
  in
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 10. (fun () -> Ok (loop ()))
  with
  | Ok () -> Buffer.contents written
  | Error `Timeout -> Alcotest.fail "the end mark was not read in 10 s"

let with_null f =
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null) (fun () -> f null)

(* Standard error is a terminal and standard output is not: the region is
   drawn, since the display draws on standard error. An implementation that
   asks standard output draws history here. *)
let test_auto_asks_the_formatters_own_descriptor () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  with_null @@ fun null ->
  let err_tty, out_tty, auto =
    with_fds ~stdout:null ~stderr:tty ~term:"xterm-256color" (fun () ->
        let asked =
          ( Console_eio.is_tty Unix.stderr,
            Console_eio.is_tty Unix.stdout,
            Console_eio.auto Fmt.stderr )
        in
        draw_a_task ();
        mark Unix.stderr;
        asked)
  in
  Alcotest.(check bool) "standard error is a terminal" true err_tty;
  Alcotest.(check bool) "standard output is not" false out_tty;
  Alcotest.(check bool) "auto on standard error" true (auto = `Activity);
  let written = read_to_mark env pty in
  Alcotest.(check bool)
    "a live region on standard error's terminal" true
    (Astring.String.is_infix ~affix:hide_cursor written)

(* Standard error is a pipe and standard output a terminal: the display
   appends, and the pipe holds the task's line and no escape. An
   implementation that asks standard output draws a region into the pipe. *)
let test_auto_appends_on_a_pipe () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () -> Unix.close r) @@ fun () ->
  let auto =
    Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
    with_fds ~stdout:tty ~stderr:w ~term:"xterm-256color" (fun () ->
        let auto = Console_eio.auto Fmt.stderr in
        draw_a_task ();
        auto)
  in
  Alcotest.(check bool) "auto on standard error" true (auto = `History);
  let written = Buffer.create 256 and buf = Bytes.create 4096 in
  let rec drain () =
    match Unix.read r buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes written buf 0 n;
        drain ()
  in
  drain ();
  let written = Buffer.contents written in
  Alcotest.(check bool)
    "no escape" false
    (Astring.String.is_infix ~affix:"\027[" written);
  Alcotest.(check string) "the task's line" "=> step  0.0s\n" written

(* Both descriptors are the same terminal; under TERM=xterm-256color the
   region is drawn, under TERM=dumb the display appends. *)
let test_auto_appends_under_term_dumb () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  let draw term =
    with_fds ~stdout:tty ~stderr:tty ~term (fun () ->
        draw_a_task ();
        mark Unix.stderr);
    read_to_mark env pty
  in
  Alcotest.(check bool)
    "a live region under xterm-256color" true
    (Astring.String.is_infix ~affix:hide_cursor (draw "xterm-256color"));
  let dumb = draw "dumb" in
  Alcotest.(check bool)
    "no escape under dumb" false
    (Astring.String.is_infix ~affix:"\027[" dumb);
  Alcotest.(check bool)
    "the task's line under dumb" true
    (Astring.String.is_infix ~affix:"=> step  0.0s" dumb)

(* A formatter of the caller's own on standard error's channel names no
   descriptor, so [`Auto] appends; with [~fd] naming standard error, the
   region is drawn on its terminal. *)
let test_a_formatter_of_ones_own_says_its_descriptor () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  with_null @@ fun null ->
  let draw ?fd () =
    let ppf = Format.formatter_of_out_channel Stdlib.stderr in
    with_fds ~stdout:null ~stderr:tty ~term:"xterm-256color" (fun () ->
        let mock = Eio_mock.Clock.make () in
        let clock = (mock :> float Eio.Time.clock_ty Eio.Resource.t) in
        Console_eio.run ~clock ~ppf ?fd ~mode:`Auto (fun display ->
            Console.Display.succeed (Console.Display.task display "step"));
        mark Unix.stderr);
    read_to_mark env pty
  in
  let unnamed = draw () in
  Alcotest.(check bool)
    "no escape without ~fd" false
    (Astring.String.is_infix ~affix:"\027[" unnamed);
  Alcotest.(check bool)
    "a live region with ~fd" true
    (Astring.String.is_infix ~affix:hide_cursor (draw ~fd:Unix.stderr ()))

let suite =
  ( "auto",
    [
      Alcotest.test_case "auto asks the formatter's own descriptor" `Quick
        test_auto_asks_the_formatters_own_descriptor;
      Alcotest.test_case "auto appends on a pipe" `Quick
        test_auto_appends_on_a_pipe;
      Alcotest.test_case "auto appends under TERM=dumb" `Quick
        test_auto_appends_under_term_dumb;
      Alcotest.test_case "a formatter of one's own says its descriptor" `Quick
        test_a_formatter_of_ones_own_says_its_descriptor;
    ] )
