(** Leave a live {!Console.Display} on a terminal the way a command does when it
    fails inside it: [exit] (what a CLI's error path calls) or an exception that
    escapes to the toplevel. The display writes its control characters to
    stdout, so a cram test reads the bytes the terminal was left with. *)

let () =
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter
      ~now:(fun () -> 0.)
      ~wait:ignore
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:true ()
  in
  Console.Display.run ~ctx ~mode:`Activity (fun display ->
      let task = Console.Display.task display "pushing" in
      Console.Display.tick display;
      match Sys.argv with
      | [| _; "exit" |] -> exit 3
      | [| _; "raise" |] -> failwith "push failed"
      | _ -> Console.Display.succeed task)
