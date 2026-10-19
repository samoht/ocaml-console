(** Render a {!Console.Display} TTY session (real control characters) through
    Matrix's terminal emulator and print the reconstructed screen after each
    frame, so a cram test can pin exactly what a terminal shows: nested rows
    with log windows, rows being finished, fresh logs, and the redraw in
    between.

    A fixed clock keeps spinners and timers deterministic. Width and height are
    taken from [$COLUMNS]/[$LINES] (set by the cram test) so the geometry -- and
    therefore the wrapping and the height cap -- is stable. *)

let geom name default =
  match Option.bind (Sys.getenv_opt name) int_of_string_opt with
  | Some v -> v
  | None -> default

let () =
  let height = geom "LINES" 24 in
  let width = geom "COLUMNS" 80 in
  let vte = Vte.create ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~wait:ignore
      ~dimensions:(fun () -> (width, height))
      ~is_tty:true ()
  in
  let sync () =
    Format.pp_print_flush ppf ();
    Vte.feed_string vte (Buffer.contents buf);
    Buffer.clear buf
  in
  (* A frame is what the driver's tick painted. An update of its own repaints
     at the eye's pace, which on this pinned clock is never, so a frame taken
     without the tick shows the region as the previous one left it. *)
  let frame ?display title =
    Option.iter Console.Display.tick display;
    sync ();
    Fmt.pr "--- %s ---@." title;
    List.iter
      (fun l -> Fmt.pr "%s@." l)
      (Console_vte.screen ~scrollback:true vte)
  in
  let module P = Console.Display in
  P.run ~ctx ~mode:`Activity ~height ~header:"Build" (fun display ->
      let frame title = frame ~display title in
      (* A tree: app -> {resolve, compile}, each with a small log window. *)
      let app = P.task ~log_lines:2 display "app" in
      P.log app "planning";
      frame "tree with one node + a log";
      let resolve = P.task ~parent:app ~log_lines:2 display "resolve" in
      P.log resolve "fetch index";
      let compile = P.task ~parent:app ~log_lines:3 display "compile" in
      P.log compile "cc a.o";
      P.log compile "cc b.o";
      frame "two children, each streaming logs";
      P.succeed resolve;
      frame "resolve finished (committed), siblings still live";
      P.log compile "cc c.o";
      P.log compile "link";
      P.log app "still working";
      frame "more logs after the row closed";
      P.succeed compile;
      P.succeed app);
  frame "all finished"
