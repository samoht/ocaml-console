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
  Progress_driver.show ~write_error

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
        "\u{2713} fine     0.0s";
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

let contains affix s = Astring.String.is_infix ~affix s

(* The clock stays at 0 here, so that no row is drawn: what shows of a task is
   what it leaves. *)
let test_commit () =
  let out commit =
    shown (fun ppf root -> driver () ~progress:`Tty ~size ?commit ppf root)
  in
  Alcotest.(check bool) "its tree" true (contains "fine" (out None));
  let left t = Some (View.text ("left " ^ Task.label t)) in
  Alcotest.(check bool)
    "what is given" true
    (contains "left fine" (out (Some left)));
  let none _ = None in
  Alcotest.(check bool) "nothing" false (contains "fine" (out (Some none)))

(* The rows of a task that runs while the clock moves, and nothing it leaves. *)
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
    Alcotest.(check bool) label expected (contains "fine" out)
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

(* A view that raises is the program's, whatever [write_error] says. *)
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
      Alcotest.test_case "a view that raises" `Quick test_view_raises;
    ] )
