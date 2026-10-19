(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A build in the terminal's scrollback: steps that end are printed once and
   stay; only the steps still running are redrawn in place. Pipe it through
   [cat] to see the lines a CI log gets. *)

let fetch clock display =
  let t = Console.Display.task ~log_lines:2 display "fetch dependencies" in
  let total = Int64.mul 48L 1048576L in
  for i = 0 to 12 do
    Console.Display.set_bytes t
      ~cur:(Int64.div (Int64.mul total (Int64.of_int i)) 12L)
      ~total;
    if i mod 4 = 0 then Fmt.kstr (Console.Display.log t) "GET pkg-%d.tar.gz" i;
    Eio.Time.sleep clock 0.15
  done;
  Console.Display.succeed t

let compile clock display =
  let t = Console.Display.task ~log_lines:3 display "compile" in
  let files = [| "lexer.ml"; "parser.ml"; "types.ml"; "eval.ml"; "main.ml" |] in
  Array.iteri
    (fun i f ->
      Console.Display.set_count t ~cur:i ~total:(Array.length files);
      Fmt.kstr (Console.Display.log t) "ocamlopt %s" f;
      Eio.Time.sleep clock 0.35)
    files;
  Console.Display.set_count t ~cur:(Array.length files)
    ~total:(Array.length files);
  Console.Display.log ~level:`Warning t "eval.ml: unused variable x";
  Console.Display.succeed t

let test clock display =
  let t = Console.Display.task display "test" in
  let unit = Console.Display.task ~parent:t display "unit" in
  let fuzz = Console.Display.task ~parent:t display "properties" in
  for i = 0 to 10 do
    Console.Display.set_count unit ~cur:(i * 12) ~total:120;
    Console.Display.set_count fuzz ~cur:i ~total:10;
    Eio.Time.sleep clock 0.2
  done;
  Console.Display.succeed t

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock ~header:"Building" (fun display ->
      Eio.Fiber.both
        (fun () -> fetch clock display)
        (fun () -> compile clock display);
      test clock display;
      Console.Display.set_result display (`Ok "build complete"))
