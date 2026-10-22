(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A build in the terminal's scrollback: steps that end are printed once and
   stay; only the steps still running are redrawn in place, under a title in
   the theme's palette and a table of the steps, whose frame moves in a theme
   with motion, and above the summary. The title, the table and the summary
   are printed once the build is done. Name a theme to see it in that one;
   pipe it through [cat] to see the lines a CI log gets. *)

open Console

(* The compiler's warnings, which stay above the rows once their step is done.
*)
let src = Logs.Src.create "ocamlopt" ~doc:"The compiler's diagnostics"

type step = { name : string; mutable status : View.t; mutable detail : string }

let step name = { name; status = View.text "waiting"; detail = "" }
let steps = [ step "fetch"; step "compile"; step "test" ]

let title =
  View.styled Style.bold
    (View.gradient (View.text "console, built step by step"))

let table () =
  Table.v ~rules:true
    [
      Table.column (View.text "Step");
      Table.column ~min_width:7 (View.text "Status");
      Table.column ~min_width:19 (View.text "Detail");
    ]
    (List.map
       (fun s -> [ View.text s.name; s.status; View.text s.detail ])
       steps)

(* What a step's row of the table says, from start to end. *)
let started name =
  let s = List.find (fun s -> String.equal s.name name) steps in
  s.status <- View.role `Accent (View.text "running");
  fun detail ->
    s.status <- View.styled Style.bold (View.role `Ok (View.text "done"));
    s.detail <- detail

(* What a step's commands print shows under its row while it runs, and goes
   when it succeeds. *)
let fetch clock display =
  let finished = started "fetch" in
  Display.with_task ~log_lines:2 display "fetch dependencies" @@ fun t ->
  let total = Int64.mul 48L 1048576L in
  for i = 0 to 12 do
    Display.set_bytes t
      ~cur:(Int64.div (Int64.mul total (Int64.of_int i)) 12L)
      ~total;
    if i mod 4 = 0 then Fmt.kstr (Display.log t) "GET pkg-%d.tar.gz" i;
    Eio.Time.Mono.sleep clock 0.15
  done;
  finished "13 packages, 48 MiB"

let compile clock display =
  let finished = started "compile" in
  let files = [| "lexer.ml"; "parser.ml"; "types.ml"; "eval.ml"; "main.ml" |] in
  Display.with_task ~log_lines:3 display "compile" (fun t ->
      Array.iteri
        (fun i f ->
          Display.set_count t ~cur:i ~total:(Array.length files);
          Fmt.kstr (Display.log t) "ocamlopt %s" f;
          Eio.Time.Mono.sleep clock 0.35)
        files;
      Display.set_count t ~cur:(Array.length files) ~total:(Array.length files));
  (* Logged outside the task, the warning outlives its success. *)
  Logs.warn ~src (fun m -> m "eval.ml: unused variable x");
  finished "5 files, 1 warning"

let test clock display =
  let finished = started "test" in
  Display.with_task display "test" @@ fun _ ->
  let unit = Display.task display "unit" in
  let fuzz = Display.task display "properties" in
  for i = 0 to 10 do
    Display.set_count unit ~cur:(i * 12) ~total:120;
    Display.set_count fuzz ~cur:i ~total:10;
    Eio.Time.Mono.sleep clock 0.2
  done;
  finished "130 passed"

(* The title and the table above the rows and the summary; they give way
   first when the rows need the room. *)
let view display ~width ~height =
  View.vcat
    [
      title;
      Table.to_span ~width:(min width 100) (table ());
      Display.default_view display ~width ~height;
    ]

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  let theme =
    match Theme.of_string name with
    | Ok theme -> theme
    | Error (`Msg e) ->
        prerr_endline e;
        exit 2
  in
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~summary:"Building" ~view (fun display ->
      Eio.Fiber.both
        (fun () -> fetch clock display)
        (fun () -> compile clock display);
      test clock display;
      Display.set_result display (Ok "build complete"))
