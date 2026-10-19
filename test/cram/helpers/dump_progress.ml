(** Demonstrates Console.Display in plain mode: a 3-step job whose finished rows
    are committed as append-only lines, then a summary. A fixed clock keeps the
    timers deterministic so cram diffs are stable. *)

let () =
  let now = ref 0. in
  let display =
    Console.Display.v ~header:"Processing"
      ~now:(fun () -> !now)
      Format.std_formatter
  in
  let step name =
    let task = Console.Display.task display name in
    now := !now +. 0.1;
    Console.Display.succeed task
  in
  step "foo.ml";
  step "bar.ml";
  step "baz.ml";
  Console.Display.close display
