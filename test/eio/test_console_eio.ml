(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

let with_clock f = Eio_main.run @@ fun _env -> f (Eio_mock.Clock.Mono.make ())

let buffer () =
  let buf = Buffer.create 64 in
  (buf, Format.formatter_of_buffer buf)

let test_run_returns () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      let result = Console_eio.run ~clock ~ppf (fun _display -> 42) in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "nothing on a pipe" "" (Buffer.contents buf))

let seconds s = Mtime.of_uint64_ns (Int64.of_float (s *. 1e9))

(* What a task that waits two seconds on a mock monotonic clock prints. *)
let nap () =
  Eio_main.run @@ fun _ ->
  let clock = Eio_mock.Clock.Mono.make () in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Display.with_task display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () -> Eio.Time.Mono.sleep clock 2.)
            (fun () -> Eio_mock.Clock.Mono.set_time clock (seconds 2.))));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let took s out = contains ("\u{2713} nap  " ^ s) out

(* A display reads the time on its monotonic clock. *)
let test_run_on_a_monotonic_clock () =
  Alcotest.(check bool) "the time it took" true (took "2.0s" (nap ()))

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

(* What a task prints that sleeps 1.5 and 0.5 seconds on [Console_eio.clock]
   of a mock clock, which moves to the end of each sleep in turn. *)
let sleeps () =
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let buf, ppf = buffer () in
  let slept = ref false in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Display.with_task display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () ->
              Eio.Time.Mono.sleep clock 1.5;
              Eio.Time.Mono.sleep clock 0.5;
              slept := true)
            (fun () ->
              while not !slept do
                Eio.Fiber.yield ();
                Eio_mock.Clock.Mono.advance mock
              done)));

  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* What a task prints whose two fibers sleep 1 and 2 seconds at once on
   [Console_eio.clock] of a mock clock. *)
let overlapping () =
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let buf, ppf = buffer () in
  let slept = ref 0 in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Display.with_task display "nap" (fun _ ->
          Eio.Fiber.all
            [
              (fun () ->
                Eio.Time.Mono.sleep clock 1.;
                incr slept);
              (fun () ->
                Eio.Time.Mono.sleep clock 2.;
                incr slept);
              (fun () ->
                while !slept < 2 do
                  Eio.Fiber.yield ();
                  Eio_mock.Clock.Mono.advance mock
                done);
            ]));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* CONSOLE_DETERMINISTIC_CLOCK, set and not empty, makes the display read
   the time of the sleeps on a clock of [Console_eio.clock], and zero on
   another clock; the waits still run on the clock behind it. *)
let test_deterministic_clock () =
  let deterministic f =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] (fun () -> f ())
  in
  Alcotest.(check bool) "a moving clock" true (took "2.0s" (nap ()));
  Alcotest.(check bool)
    "another clock stops" true
    (took "0.0s" (deterministic nap));
  let first = deterministic sleeps in
  Alcotest.(check bool) "one sleep after the other" true (took "2.0s" first);
  Alcotest.(check string) "the same twice" first (deterministic sleeps);
  Alcotest.(check bool)
    "sleeps at once overlap" true
    (took "2.0s" (deterministic overlapping));
  let empty =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "") ] (fun () -> nap ())
  in
  Alcotest.(check bool) "an empty value" true (took "2.0s" empty)

let test_run_raises () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun display ->
              ignore (Console.Display.task display "doomed");
              failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the running task is cancelled" true
        (contains "\xe2\x97\x8b doomed" (Buffer.contents buf)))

let test_nested_run_reuses_the_display () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              Console.Display.succeed (Console.Display.task inner "nested work")));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses the outer output" true
        (String.starts_with
           ~prefix:"\xe2\x80\xa2 nested work\n\xe2\x9c\x93 nested work"
           (Buffer.contents buf)))

(* A display in another domain is refused, and the one that runs is not
   disturbed. *)
let test_second_display_refused () =
  Eio_main.run @@ fun env ->
  let clock = Eio_mock.Clock.Mono.make () in
  let _, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun _ ->
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
          Alcotest.check_raises "refused"
            (Invalid_argument "Console_eio.run: a display is already running")
            (fun () -> Console_eio.run ~clock ~ppf ignore)));
  Console_eio.run ~clock ~ppf ignore

let test_dimensions_read_the_environment () =
  let check msg columns lines expected =
    with_env
      [ ("COLUMNS", columns); ("LINES", lines) ]
      (fun () ->
        Alcotest.(check (pair int int)) msg expected (Terminal.dimensions None))
  in
  check "the exported size" "123" "40" (123, 40);
  check "a small size" "10" "2" (10, 2);
  check "zero is no answer" "0" "0" (80, 24);
  check "nor is none" "" "" (80, 24)

(* {1 On a terminal} *)

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

(* What [teardown.exe how] writes on a terminal, how it exits, and whether
   the terminal echoes what is typed once it has. With [continue], the child
   is sent SIGCONT every tenth of a second, as a shell's [fg] does, until it
   exits. *)
let session ?(no_color = false) ?(lang = "en_US.UTF-8") ?(continue = false) how
    =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:
        [|
          "TERM=xterm-256color";
          ("NO_COLOR=" ^ if no_color then "1" else "");
          "LANG=" ^ lang;
        |]
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:"./teardown.exe" [ "teardown.exe"; how ]
  in
  let clock = Eio.Stdenv.clock env in
  let written = Buffer.create 256 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let status = ref None in
  (* The reader runs on its own, never raced against a timeout that would
     drop what a read returns as it ends; it stops once the child has exited
     and nothing more has come for half a second. *)
  let rec read () =
    match Eio.Flow.single_read source buf with
    | n ->
        Buffer.add_string written (Cstruct.to_string ~len:n buf);
        read ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  let rec settle seen =
    Eio.Time.sleep clock 0.5;
    let now = Buffer.length written in
    if now <> seen then settle now
  in
  let rec fg () =
    if continue && Option.is_none !status then begin
      Eio.Time.sleep clock 0.1;
      (try Unix.kill (Eio.Process.pid child) Sys.sigcont
       with Unix.Unix_error _ -> ());
      fg ()
    end
  in
  Eio.Fiber.first read (fun () ->
      Eio.Fiber.both (fun () -> status := Some (Eio.Process.await child)) fg;
      settle (Buffer.length written));
  let echo = Eio_unix.Fd.use_exn "tcgetattr" tty Unix.tcgetattr in
  (Option.get !status, Buffer.contents written, echo.Unix.c_echo)

let teardown how =
  let status, out, _ = session how in
  (status, out)

let hide = "\027[?25l"

let last_cursor s =
  let rec find i acc =
    match Astring.String.find_sub ~start:i ~sub:"\027[?25" s with
    | None -> acc
    | Some j when j + 5 < String.length s -> find (j + 5) (Some s.[j + 5])
    | Some _ -> acc
  in
  find 0 None

let test_teardown () =
  List.iter
    (fun (how, expected) ->
      let status, out = teardown how in
      Alcotest.(check bool) (how ^ ": hid the cursor") true (contains hide out);
      Alcotest.(check (option char))
        (how ^ ": the cursor is shown last")
        (Some 'h') (last_cursor out);
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (how ^ ": standard output above the region, unlabelled")
        true
        (List.mem "a line of output" (Console_vte.screen ~scrollback:true vte));
      Alcotest.(check int)
        (how ^ ": the cursor is at the start of a line")
        0
        (snd (Console_vte.cursor_pos vte));
      Alcotest.(check bool)
        (how ^ ": exit status") true
        (match (status, expected) with
        | `Exited a, `Exited b -> Int.equal a b
        | _ -> false))
    [
      ("ok", `Exited 0);
      ("exit", `Exited 3);
      ("raise", `Exited 2);
      ("cancel", `Exited 0);
    ]

(* The final screen of what a session wrote. *)
let final_of out =
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Console_vte.screen ~scrollback:true vte

(* The final screen of [teardown.exe how]. *)
let final how = final_of (snd (teardown how))

(* Ctrl-C cancels the task, which prints its output under its line; a task
   that succeeds drops it. *)
let test_sigint_output () =
  let rows = final "sigint" in
  let rec under = function
    | l :: next :: _ when contains "\u{25cb} pushing" l -> next
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no cancelled line"
  in
  Alcotest.(check string)
    "the output stays" "  \u{203a} pushed 3 objects" (under rows);
  Alcotest.(check bool)
    "a success drops it" false
    (List.exists (contains "pushed 3 objects") (final "ok"))

(* What a task's function prints on the display's terminal, on either
   standard output, is its output, as are the Logs messages of the fibers it
   forks: it goes when the task succeeds and is printed under its line when
   it fails. A line printed within the root task goes above the rows. *)
let test_task_output () =
  let output =
    [
      "  \u{203a} an output line";
      "  \u{203a} a result line";
      "  \u{203a} a note line";
      "  ! a forked note";
    ]
  in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l ->
        List.filteri (fun i _ -> i < List.length output) rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let failed = final "logs-failed" in
  Alcotest.(check (list string)) "under the failure" output (under failed);
  let rows = final "logs" in
  List.iter
    (fun rows ->
      Alcotest.(check bool)
        "within the root, above the rows" true
        (List.mem "a root line" rows))
    [ failed; rows ];
  Alcotest.(check bool)
    "the success line" true
    (List.exists (contains "\u{2713} work") rows);
  List.iter
    (fun text ->
      Alcotest.(check bool)
        ("folded away: " ^ text) false
        (List.exists (contains text) rows))
    [ "an output line"; "a result line"; "a note line"; "a forked note" ]

(* A line written outside a task is not the output of the task in which
   the next line is written, even when it waited for a flush until then. *)
let test_held_line () =
  let rows = final "held" in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l -> rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let output = List.filter (contains "\u{203a}") (under rows) in
  Alcotest.(check (list string))
    "the task's line alone under it"
    [ "  \u{203a} a task line" ]
    output;
  Alcotest.(check bool)
    "the held line above the rows" true
    (List.exists (String.starts_with ~prefix:"a held line") rows)

let test_sigint_teardown () =
  let _, out = teardown "sigint" in
  Alcotest.(check bool)
    "SIGINT interrupted a live display" true (contains hide out);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "SIGINT restores the cursor" true
    (Console_vte.cursor_visible vte)

(* However a session ends, text written after it is drawn as on a fresh
   terminal: at the first column, in the terminal's own colours. *)
let test_usable_after () =
  List.iter
    (fun (how, no_color) ->
      let _, out, _ = session ~no_color how in
      let name = Fmt.str "%s%s" how (if no_color then " (NO_COLOR)" else "") in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (name ^ ": cursor shown") true
        (Console_vte.cursor_visible vte);
      let y, x = Console_vte.cursor_pos vte in
      Alcotest.(check int) (name ^ ": first column") 0 x;
      Console_vte.feed vte "AFTER";
      let plain = Console_vte.v ~rows:1 ~cols:80 () in
      Console_vte.feed plain "AFTER";
      let g = Console_vte.grid vte and reference = Console_vte.grid plain in
      let i = Grid.idx g ~x:0 ~y and j = Grid.idx reference ~x:0 ~y:0 in
      let colours g i =
        List.concat_map
          (fun c ->
            let r, g, b, a = Ansi.Color.to_rgba c in
            [ r; g; b; a ])
          [ Console_vte.fg g i; Console_vte.bg g i ]
      in
      Alcotest.(check (list int))
        (name ^ ": default colours")
        (colours reference j) (colours g i))
    (List.concat_map
       (fun how -> [ (how, false); (how, true) ])
       [ "ok"; "raise"; "cancel"; "exit"; "sigint" ])

(* A terminal whose locale is not UTF-8 shows other characters as garbage:
   every widget and the display are then drawn with ASCII alone. *)
let test_ascii_locale () =
  let _, out, _ = session ~lang:"C" "widgets" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool)
    "ASCII only" true
    (String.for_all (fun c -> Char.code c < 0x80) out)

(* A program that chose its colours before [run] keeps them. *)
let test_choice_kept () =
  let _, out, _ = session "colourless" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool) "no colour" false (contains "\027[0m" out)

(* A theme is not a colour choice: a program that only chose a theme still
   has colour decided for it, and in that theme. *)
let test_theme_not_a_choice () =
  let _, out, _ = session "themed" in
  let nord =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Console.Color.set_depth ppf `Ansi_256;
    Console.(
      View.pp ppf
        (View.styled
           Style.(bold + fg (Theme.succeeded Theme.nord))
           (View.text "x")));
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    String.sub s 0 (String.index s 'x')
  in
  Alcotest.(check bool) "colour, in the theme" true (contains nord out)

(* Ctrl-Z gives the terminal back before the program stops: the region is
   erased and the cursor shown. Once continued, the region is drawn again and
   the work goes on. *)
let test_suspend () =
  let status, out, _ = session ~continue:true "suspend" in
  let show = "\027[?25h" in
  let hidden = Option.get (Astring.String.find_sub ~sub:hide out) in
  match Astring.String.find_sub ~start:hidden ~sub:show out with
  | None -> Alcotest.fail "the cursor is not shown before the stop"
  | Some stop ->
      let before = String.sub out 0 (stop + String.length show) in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte before;
      Alcotest.(check bool)
        "the cursor is shown" true
        (Console_vte.cursor_visible vte);
      Alcotest.(check bool)
        "the region is erased" false
        (List.exists (contains "pausing") (Console_vte.screen vte));
      Alcotest.(check bool)
        "the region is drawn again, the cursor hidden" true
        (Option.is_some (Astring.String.find_sub ~start:stop ~sub:hide out));
      Alcotest.(check bool)
        "the work goes on" true
        (List.exists (contains "\u{2713} pausing") (final_of out));
      Alcotest.(check bool)
        "exit status" true
        (match status with `Exited 0 -> true | _ -> false)

(* Output outside a task that the program never flushes is not held
   without bound: all but the last 64 KiB of it is on the terminal before
   the program goes on. *)
let test_unflushed_output_is_bounded () =
  let _, out, _ = session "unflushed" in
  match Astring.String.find_sub ~sub:"MARK" out with
  | None -> Alcotest.fail "no mark"
  | Some k ->
      let after = String.length out - k in
      let held =
        List.length (Astring.String.cuts ~sep:"line " (String.sub out k after))
        - 1
      in
      Alcotest.(check bool)
        (Fmt.str "at most 64 KiB held back (%d lines)" held)
        true
        (held * String.length "line 000000\n" <= 65536)

let test_sigint_secret () =
  let status, _, echo = session "secret" in
  Alcotest.(check bool)
    "SIGINT still ends the program" true
    (match status with `Signaled s -> Int.equal s Sys.sigint | _ -> false);
  Alcotest.(check bool) "SIGINT during a password restores echo" true echo

let test_quiet_terminal_capture () =
  let status, out = teardown "quiet-ok" in
  Alcotest.(check bool)
    "the program succeeds" true
    (match status with `Exited 0 -> true | _ -> false);
  Alcotest.(check (list string))
    "successful task output is hidden; root output stays" [ "root line" ]
    (final_of out)

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "quiet hides successful terminal output" `Quick
        test_quiet_terminal_capture;
      Alcotest.test_case "the terminal is usable after every ending" `Quick
        test_usable_after;
      Alcotest.test_case "the program's colour choice is kept" `Quick
        test_choice_kept;
      Alcotest.test_case "a theme is not a colour choice" `Quick
        test_theme_not_a_choice;
      Alcotest.test_case "ASCII on a terminal that is not UTF-8" `Quick
        test_ascii_locale;
      Alcotest.test_case "SIGINT during a password restores echo" `Quick
        test_sigint_secret;
      Alcotest.test_case "unflushed output is held back boundedly" `Quick
        test_unflushed_output_is_bounded;
      Alcotest.test_case "SIGINT restores the terminal cursor" `Quick
        test_sigint_teardown;
      Alcotest.test_case "Ctrl-Z gives the terminal back, fg redraws" `Quick
        test_suspend;
      Alcotest.test_case "SIGINT prints a cancelled task's output" `Quick
        test_sigint_output;
      Alcotest.test_case "a task's output on a terminal" `Quick test_task_output;
      Alcotest.test_case "a held line stays outside the next task" `Quick
        test_held_line;
      Alcotest.test_case "run returns" `Quick test_run_returns;
      Alcotest.test_case "run on a monotonic clock" `Quick
        test_run_on_a_monotonic_clock;
      Alcotest.test_case "deterministic clock" `Quick test_deterministic_clock;
      Alcotest.test_case "run raises" `Quick test_run_raises;
      Alcotest.test_case "nested run reuses the display" `Quick
        test_nested_run_reuses_the_display;
      Alcotest.test_case "a display in another domain is refused" `Quick
        test_second_display_refused;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "teardown on a terminal" `Quick test_teardown;
    ] )
