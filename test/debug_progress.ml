(* Debug demo for the progress display - run interactively to see behaviour.

   Usage: dune exec ocaml-console/test/debug_progress.exe (auto: tty or plain) dune
   exec ocaml-console/test/debug_progress.exe -- plain (force plain) *)

let mode =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "plain" then `Plain else `Auto

let () =
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter ~now:Unix.gettimeofday
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:true ()
  in
  let d = Console.Display.v ~ctx ~mode ~header:"Building" () in
  let step = Console.Display.row ~log_lines:4 d "RUN make" in
  for i = 0 to 5 do
    Console.Display.set_count step ~cur:i ~total:5;
    Fmt.kstr (Console.Display.log step) "  CC src/file%d.c" i;
    Unix.sleepf 0.2;
    Console.Display.tick d
  done;
  Console.Display.finish step;
  let layer = Console.Display.row ~parent:step d "sha256:abcd…" in
  let total = 2147483648L in
  for i = 0 to 8 do
    Console.Display.set_bytes layer
      ~cur:(Int64.mul (Int64.of_int i) 268435456L)
      ~total;
    Unix.sleepf 0.2;
    Console.Display.tick d
  done;
  Console.Display.finish layer;
  Console.Display.finish_all d
