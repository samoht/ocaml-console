(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A build in the terminal's scrollback: steps that end are printed once and
   stay; only the steps still running are redrawn in place, under a table of
   the steps, whose frame moves in a theme with motion. The table is printed
   once the build is done. Name a theme to see it in that one; pipe it
   through [cat] to see the lines a CI log gets. *)

open Console

type step = { name : string; mutable status : Span.t; mutable detail : string }

let step name = { name; status = Span.text "waiting"; detail = "" }
let steps = [ step "fetch"; step "compile"; step "test" ]

let table () =
  Table.v
    [ Table.column "Step"; Table.column "Status"; Table.column "Detail" ]
    (List.map
       (fun s -> [ Span.text s.name; s.status; Span.text s.detail ])
       steps)

(* What a step's row of the table says, from start to end. *)
let started theme name =
  let s = List.find (fun s -> String.equal s.name name) steps in
  s.status <- Span.styled (Style.fg (Theme.accent theme)) "running";
  fun detail ->
    s.status <- Span.styled Style.(bold + fg (Theme.ok theme)) "done";
    s.detail <- detail

let fetch theme clock display =
  let finished = started theme "fetch" in
  let t = Display.task ~log_lines:2 display "fetch dependencies" in
  let total = Int64.mul 48L 1048576L in
  for i = 0 to 12 do
    Display.set_bytes t
      ~cur:(Int64.div (Int64.mul total (Int64.of_int i)) 12L)
      ~total;
    if i mod 4 = 0 then Fmt.kstr (Display.log t) "GET pkg-%d.tar.gz" i;
    Eio.Time.sleep clock 0.15
  done;
  finished "13 packages, 48 MiB";
  Display.succeed t

let compile theme clock display =
  let finished = started theme "compile" in
  let t = Display.task ~log_lines:3 display "compile" in
  let files = [| "lexer.ml"; "parser.ml"; "types.ml"; "eval.ml"; "main.ml" |] in
  Array.iteri
    (fun i f ->
      Display.set_count t ~cur:i ~total:(Array.length files);
      Fmt.kstr (Display.log t) "ocamlopt %s" f;
      Eio.Time.sleep clock 0.35)
    files;
  Display.set_count t ~cur:(Array.length files) ~total:(Array.length files);
  Display.log ~level:`Warning t "eval.ml: unused variable x";
  finished "5 files, 1 warning";
  Display.succeed t

let test theme clock display =
  let finished = started theme "test" in
  let t = Display.task display "test" in
  let unit = Display.task ~parent:t display "unit" in
  let fuzz = Display.task ~parent:t display "properties" in
  for i = 0 to 10 do
    Display.set_count unit ~cur:(i * 12) ~total:120;
    Display.set_count fuzz ~cur:i ~total:10;
    Eio.Time.sleep clock 0.2
  done;
  finished "130 passed";
  Display.succeed t

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  let theme =
    match Theme.of_string name with
    | Ok theme -> theme
    | Error (`Msg e) ->
        prerr_endline e;
        exit 2
  in
  List.iter (fun ppf -> Theme.set ppf theme) [ Fmt.stdout; Fmt.stderr ];
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock ~header:"Building" (fun display ->
      Display.set_scene display (fun ~height:_ ppf -> Table.pp ppf (table ()));
      Eio.Fiber.both
        (fun () -> fetch theme clock display)
        (fun () -> compile theme clock display);
      test theme clock display;
      Display.set_result display (Ok "build complete"));
  Fmt.pr "%a@." Table.pp (table ())
