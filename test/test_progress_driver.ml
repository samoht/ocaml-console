(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
module Progress_driver = Console__.Progress_driver

type show =
  ?progress:Progress.t ->
  ?layout:Progress.Layout.t ->
  ?view:View.t ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task.t ->
  Display.t

let driver ?(write_error = fun _ -> false) () : show =
  Progress_driver.show ~write_error ?cancelled:None ?atomic:None

let public : show = Progress.show

(* A task that succeeds, one that fails, each with a line, under a header;
   the root's state once the display closed. *)
let work display ppf =
  let root = Task.root ~label:"Building" ~now:(fun () -> 0.) () in
  let d = display ppf root in
  Task.run root "fine" (fun t -> Task.log t "quiet line");
  (try
     Task.run root "broken" (fun t ->
         Task.log t "loud line";
         failwith "broken")
   with Failure _ -> ());
  Task.succeed root;
  Display.close d;
  Task.state root

let shown display =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let (_ : Task.state) = work display ppf in
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let size () = (40, 10)

(* While the output takes every write, [show] prints what [Progress.show]
   prints, in each form, whatever [write_error] would take. *)
let test_same_as_public () =
  let same label display =
    Alcotest.(check string)
      label
      (shown (fun ppf root -> display public ppf root))
      (shown (fun ppf root ->
           display (driver ~write_error:(fun _ -> true) ()) ppf root))
  in
  let form label progress =
    same label (fun (show : show) ppf root -> show ~progress ppf root);
    same (label ^ " with a size") (fun (show : show) ppf root ->
        show ~progress ~size ppf root)
  in
  form "auto" `Auto;
  form "tty" `Tty;
  form "plain" `Plain;
  form "quiet" `Quiet;
  form "json" `Json;
  same "the default" (fun (show : show) ppf root -> show ppf root)

let lines l = String.concat "\n" l ^ "\n"

(* What the work prints as lines, before the last frame. *)
let printed =
  [
    "\u{2022} fine";
    "  \u{203a} fine: quiet line";
    "\u{2713} fine  0.0s";
    "\u{2022} broken";
    "  \u{203a} broken: loud line";
    "\u{2717} broken: Failure(\"broken\")  0.0s  failed";
  ]

let last_frame = [ "\u{2717} Building 0.0s (2/2)" ]

let check_shown label expected display =
  Alcotest.(check string) label (lines expected) (shown display)

let test_plain_exact () =
  check_shown "plain" (printed @ last_frame) (fun ppf root ->
      driver () ~progress:`Plain ppf root)

let test_quiet_exact () =
  check_shown "quiet"
    [
      "\u{2717} broken: Failure(\"broken\")  0.0s  failed";
      "  \u{203a} loud line";
    ] (fun ppf root -> driver () ~progress:`Quiet ppf root)

let test_json_exact () =
  check_shown "json"
    [
      {|{"time":0,"event":"start","id":0,"task":["fine"]}|};
      {|{"time":0,"event":"output","id":0,"task":["fine"],"text":"quiet line"}|};
      {|{"time":0,"event":"end","id":0,"task":["fine"],"status":"succeeded","elapsed":0}|};
      {|{"time":0,"event":"start","id":1,"task":["broken"]}|};
      {|{"time":0,"event":"output","id":1,"task":["broken"],"text":"loud line"}|};
      {|{"time":0,"event":"end","id":1,"task":["broken"],"status":"failed","elapsed":0,"message":"broken: Failure(\"broken\")"}|};
      {|{"time":0,"event":"end","status":"succeeded","elapsed":0}|};
    ] (fun ppf root -> driver () ~progress:`Json ppf root)

(* Each option reaches the display: what is printed with it, exact. *)
let test_view () =
  check_shown "the last frame" (printed @ [ "mine" ]) (fun ppf root ->
      driver () ~progress:`Plain ~view:(View.text "mine") ppf root)

let test_keep_ended () =
  check_shown "kept to the last frame"
    (printed
    @ [
        "\u{2713} fine                        0.0s";
        "\u{2717} broken: Failure(\"broken\")   0.0s";
        "  \u{203a} loud line";
      ]
    @ last_frame)
    (fun ppf root -> driver () ~progress:`Plain ~keep_ended:true ppf root)

let test_plain_option () =
  let line e =
    match Task.Event.kind e with `End -> Some (View.text "an end") | _ -> None
  in
  check_shown "the program's lines"
    ([ "an end"; "an end" ] @ last_frame)
    (fun ppf root -> driver () ~progress:`Plain ~plain:line ppf root)

let test_size () =
  check_shown "as wide as the terminal" (printed @ [ "\u{2717} Building" ])
    (fun ppf root ->
      driver () ~progress:`Plain ~size:(fun () -> (12, 10)) ppf root)

(* The clock stays at 0, so no row is drawn and the output holds only what
   [commit] leaves of each task. *)
let test_commit () =
  let out commit =
    shown (fun ppf root -> driver () ~progress:`Tty ~size ?commit ppf root)
  in
  Alcotest.(check bool)
    "its tree" true
    (Display_harness.contains "fine" (out None));
  let left t = Some (View.text ("left " ^ Task.label t)) in
  Alcotest.(check bool)
    "what is given" true
    (Display_harness.contains "left fine" (out (Some left)));
  let none _ = None in
  Alcotest.(check bool)
    "nothing" false
    (Display_harness.contains "fine" (out (Some none)))

(* [rows ()] is what a display prints of a task that runs while the clock
   moves, with a [commit] that leaves nothing of it. *)
let rows ?layout ?delay () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now = ref 0. in
  let root = Task.root ~now:(fun () -> !now) () in
  let commit _ = None in
  let d = driver () ~progress:`Tty ~size ~commit ?layout ?delay ppf root in
  let task = Task.start root "fine" in
  now := 1.;
  Display.redraw d;
  Task.succeed task;
  Task.succeed root;
  Display.close d;
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let test_rows () =
  let check label expected out =
    Alcotest.(check bool) label expected (Display_harness.contains "fine" out)
  in
  check "the rows show" true (rows ());
  check "not before the delay" false (rows ~delay:10. ());
  check "after it" true (rows ~delay:0.5 ());
  check "a layout without the label" false
    (rows ~layout:Progress.Layout.spinner ())

(* A display that closes while its work runs ends it as cancelled. *)
let test_close_cancels () =
  let run ppf =
    let root = Task.root ~now:(fun () -> 0.) () in
    Display.close (driver () ppf root);
    Task.state root
  in
  let ppf = Format.formatter_of_buffer (Buffer.create 16) in
  Alcotest.(check bool) "cancelled" true (run ppf = `Cancelled)

exception Closed
exception Other

let raising e = Format.make_formatter (fun _ _ _ -> raise e) ignore

let forms =
  [ ("plain", `Plain); ("tty", `Tty); ("quiet", `Quiet); ("json", `Json) ]

let succeeded =
  Alcotest.testable
    (Fmt.of_to_string (function
      | `Running -> "running"
      | `Succeeded -> "succeeded"
      | `Failed -> "failed"
      | `Cancelled -> "cancelled"))
    ( = )

let is_closed = function Closed -> true | _ -> false

(* An exception that [write_error] accepts ends the output and nothing else,
   in each form, and [write_error] is given the exception the output raised. *)
let test_write_error_taken () =
  List.iter
    (fun (label, progress) ->
      let seen = ref [] in
      let write_error e =
        seen := e :: !seen;
        is_closed e
      in
      let state =
        work
          (fun ppf root -> driver ~write_error () ~progress ppf root)
          (raising Closed)
      in
      Alcotest.check succeeded (label ^ ": the work ends") `Succeeded state;
      Alcotest.(check bool) (label ^ ": it is asked") true (!seen <> []);
      Alcotest.(check bool)
        (label ^ ": about that exception")
        true
        (List.for_all is_closed !seen))
    forms

(* An exception that [write_error] refuses reaches the program, and so does
   every exception when there is no [write_error]. *)
let test_write_error_not_taken () =
  List.iter
    (fun (label, progress) ->
      let display ppf root =
        driver ~write_error:is_closed () ~progress ppf root
      in
      Alcotest.check_raises (label ^ ": another exception") Other (fun () ->
          ignore (work display (raising Other) : Task.state));
      let display ppf root = driver () ~progress ppf root in
      Alcotest.check_raises (label ^ ": none is a write error") Closed
        (fun () -> ignore (work display (raising Closed) : Task.state));
      (* [Progress.show] has no [write_error]. *)
      let display ppf root = Progress.show ~progress ppf root in
      Alcotest.check_raises (label ^ ": as Progress.show raises") Closed
        (fun () -> ignore (work display (raising Closed) : Task.state)))
    forms

(* Whatever the driver is told, [Sys_error] is a write error on any display. *)
let test_sys_error () =
  List.iter
    (fun (label, progress) ->
      let asked = ref 0 in
      let write_error _ =
        incr asked;
        false
      in
      let state =
        work
          (fun ppf root -> driver ~write_error () ~progress ppf root)
          (raising (Sys_error "Broken pipe"))
      in
      Alcotest.check succeeded label `Succeeded state;
      Alcotest.(check int) (label ^ ": not asked") 0 !asked)
    forms

(* A flush that fails ends the output as a failing write does, whether the
   output is gone before the display opens or after. *)
let test_flush_fails () =
  List.iter
    (fun at_once ->
      List.iter
        (fun (label, progress) ->
          let closed = ref at_once in
          let flush () = if !closed then raise Closed in
          let ppf = Format.make_formatter (fun _ _ _ -> ()) flush in
          let state =
            work
              (fun ppf root ->
                let d = driver ~write_error:is_closed () ~progress ppf root in
                closed := true;
                d)
              ppf
          in
          let label = if at_once then label ^ ", closed at once" else label in
          Alcotest.check succeeded label `Succeeded state)
        forms)
    [ false; true ]

(* [with_output] still calls its function once the output fails. *)
let test_with_output_flush_fails () =
  let closed = ref false in
  let flush () = if !closed then raise (Sys_error "EPIPE") in
  let ppf = Format.make_formatter (fun _ _ _ -> ()) flush in
  let display = Display.v ~now:(fun () -> 0.) ppf View.empty in
  closed := true;
  let called = ref false in
  Display.with_output display (fun _ -> called := true);
  Alcotest.(check bool) "what it prints is asked for" true !called

(* An exception that a view raises reaches the program, even when
   [write_error] accepts every exception. *)
let test_view_raises () =
  List.iter
    (fun (label, progress) ->
      let view = View.fit (fun _ -> raise Exit) in
      Alcotest.check_raises label Exit (fun () ->
          let ppf = Format.formatter_of_buffer (Buffer.create 16) in
          let root = Task.root ~now:(fun () -> 0.) () in
          let d =
            driver ~write_error:(fun _ -> true) () ~progress ~view ppf root
          in
          Task.succeed root;
          Display.close d))
    [ ("plain", `Plain); ("tty", `Tty) ]

let suite =
  ( "progress_driver",
    [
      Alcotest.test_case "the same as Progress.show" `Quick test_same_as_public;
      Alcotest.test_case "plain" `Quick test_plain_exact;
      Alcotest.test_case "quiet" `Quick test_quiet_exact;
      Alcotest.test_case "json" `Quick test_json_exact;
      Alcotest.test_case "view" `Quick test_view;
      Alcotest.test_case "keep_ended" `Quick test_keep_ended;
      Alcotest.test_case "plain option" `Quick test_plain_option;
      Alcotest.test_case "size" `Quick test_size;
      Alcotest.test_case "commit" `Quick test_commit;
      Alcotest.test_case "delay and layout" `Quick test_rows;
      Alcotest.test_case "close cancels" `Quick test_close_cancels;
      Alcotest.test_case "a write error taken" `Quick test_write_error_taken;
      Alcotest.test_case "a write error not taken" `Quick
        test_write_error_not_taken;
      Alcotest.test_case "Sys_error" `Quick test_sys_error;
      Alcotest.test_case "a flush that fails" `Quick test_flush_fails;
      Alcotest.test_case "a flush that fails around output" `Quick
        test_with_output_flush_fails;
      Alcotest.test_case "a view that raises" `Quick test_view_raises;
    ] )
