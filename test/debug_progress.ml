(* Debug demo for the progress display - run interactively to see behaviour.

   Usage: dune exec ocaml-console/test/debug_progress.exe (auto: tty or plain) dune
   exec ocaml-console/test/debug_progress.exe -- plain (force plain) *)

let mode =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "plain" then `History
  else `Auto

let () =
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter ~now:Unix.gettimeofday
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:true ()
  in
  Console.Display.run ~ctx ~mode ~header:"Building" (fun display ->
      let step = Console.Display.task ~log_lines:4 display "RUN make" in
      let layer = Console.Display.task ~parent:step display "sha256:abcd…" in
      for i = 0 to 5 do
        Console.Display.set_count step ~cur:i ~total:5;
        Fmt.kstr (Console.Display.log step) "  CC src/file%d.c" i;
        Unix.sleepf 0.2;
        Console.Display.tick display
      done;
      let total = 2147483648L in
      for i = 0 to 8 do
        Console.Display.set_bytes layer
          ~cur:(Int64.mul (Int64.of_int i) 268435456L)
          ~total;
        Unix.sleepf 0.2;
        Console.Display.tick display
      done;
      Console.Display.succeed layer;
      Console.Display.succeed step)
