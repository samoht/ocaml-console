(** Demonstrates Console.Display in plain mode: a 3-step job whose finished rows
    are committed as append-only lines, then a summary. A fixed clock keeps the
    timers deterministic so cram diffs are stable. Regressions in the plain
    renderer will fail this cram test. *)

let () =
  let now = ref 0. in
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter
      ~now:(fun () -> !now)
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:false ()
  in
  let d = Console.Display.v ~ctx ~mode:`Plain ~header:"Processing" () in
  let step name =
    let r = Console.Display.row d name in
    now := !now +. 0.1;
    Console.Display.finish r
  in
  step "foo.ml";
  step "bar.ml";
  step "baz.ml";
  Console.Display.finish_all d
