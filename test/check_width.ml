let () =
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter
      ~now:(fun () -> 0.)
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:false ()
  in
  let width, _ = Console.Display.dimensions ctx in
  Fmt.pr "terminal_width=%d is_tty=%b\n" width (Console.Display.is_tty ctx)
