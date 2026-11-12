(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* A view that raises does not stop the close from restoring the terminal.
   The display drops the view and draws the default view instead. *)
let test_view_exception_close () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  ignore (Task.start t.root "running");
  (match Display.set_view t.display (View.fit (fun _ -> raise Exit)) with
  | () -> Alcotest.fail "view exception was swallowed"
  | exception Exit -> ());
  Alcotest.(check bool)
    "the default view drawn" true
    (List.exists (contains "running") (screen t));
  let raised =
    try
      close t;
      false
    with Exit -> true
  in
  Alcotest.(check bool)
    "closing does not call the broken view again" false raised;
  Alcotest.(check bool)
    "cursor restored after the view exception" true
    (Console_vte.cursor_visible t.vte)

(* A task ends only once, even if a function called during its end ends it
   again. There is one end event, and the first outcome stays. *)
let test_task_ends_once () =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  let root = Task.root ~now:(fun () -> 0.) () in
  let display = Progress.show ~progress:`Json ppf root in
  let task = Task.start root "t" in
  Console__.Task_driver.ending root Task.fail;
  Task.succeed task;
  Display.close display;
  Format.pp_print_flush ppf ();
  let ended =
    List.filter
      (fun l -> contains {|"event":"end"|} l && contains {|["t"]|} l)
      (String.split_on_char '\n' (Buffer.contents buf))
  in
  Alcotest.(check int) "one end" 1 (List.length ended);
  Alcotest.(check bool) "the first outcome" true (Task.state task = `Succeeded)

(* An interrupt while a task ends leaves no task half ended, whether it
   comes from the function called at the end or from a watcher. The tasks
   under it end too, and every display of the root sees each end. *)
let test_end_interrupted () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let commit _ = raise Sys.Break in
  let live =
    Progress.show ~commit
      ~size:(fun () -> (40, 10))
      (Format.formatter_of_buffer (Buffer.create 64))
      root
  in
  let buf = Buffer.create 64 in
  let other = Format.formatter_of_buffer buf in
  let watcher = Progress.show ~progress:`Plain other root in
  let parent = Task.start root "parent" in
  let first = Task.start parent "first" in
  let second = Task.start parent "second" in
  Console__.Task_driver.ending root (fun r ->
      if r == first then raise Sys.Break);
  Alcotest.check_raises "the interrupt goes on" Sys.Break (fun () ->
      Task.succeed parent);
  Alcotest.(check bool)
    "the tasks ended" true
    (List.for_all
       (fun task -> Task.state task = `Succeeded)
       [ parent; first; second ]);
  Format.pp_print_flush other ();
  List.iter
    (fun label ->
      Alcotest.(check bool)
        ("the other display saw " ^ label ^ " end")
        true
        (contains ("\xe2\x9c\x93 " ^ label ^ "  0.0s") (Buffer.contents buf)))
    [ "parent \xe2\x96\xb8 first"; "parent \xe2\x96\xb8 second"; "parent" ];
  Console__.Task_driver.ending root ignore;
  Display.close watcher;
  Display.close live

(* [breaking_size ()] is a buffer, a formatter on it, a flag [broken], and a
   terminal size function that raises [Exit] once [broken] is set. *)
let breaking_size () =
  let buf = Buffer.create 256 in
  let broken = ref false in
  let size () = if !broken then raise Exit else (40, 10) in
  (buf, Format.formatter_of_buffer buf, broken, size)

(* [check_buffer_restored ppf buf] flushes [ppf]. It checks that the output
   in [buf] hid the cursor and shows it again at the end (DECTCEM). *)
let check_buffer_restored ppf buf =
  Format.pp_print_flush ppf ();
  check_cursor_restored "cursor" (Buffer.contents buf)

(* Exceptions that the program must see at once. A view that raises one of
   them has not failed: the display re-raises it and keeps the view. *)
let urgent = [ Out_of_memory; Stack_overflow; Sys.Break ]

let breaking e =
  let broken = ref false in
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  let step = Task.start t.root "running" in
  Display.set_view t.display
    (View.fit (fun _ -> if !broken then raise e else View.text "fine"));
  (t, step, broken)

let test_view_urgent_print () =
  List.iter
    (fun e ->
      let t, _, broken = breaking e in
      broken := true;
      Alcotest.check_raises "the print" e (fun () ->
          Display.print t.display (View.text "kept"));
      Alcotest.(check bool)
        "the line printed went out" true
        (List.exists (contains "kept") (screen t));
      broken := false;
      Display.redraw t.display;
      Alcotest.(check bool)
        "the view stays" true
        (List.exists (contains "fine") (screen t));
      close t;
      Alcotest.(check bool)
        "cursor restored" true
        (Console_vte.cursor_visible t.vte))
    urgent

let test_view_urgent_report () =
  List.iter
    (fun e ->
      let t, step, broken = breaking e in
      broken := true;
      t.advance 1.;
      Alcotest.check_raises "the report" e (fun () ->
          Task.set_items step ~count:1 ~total:2);
      broken := false;
      close t)
    urgent

let test_view_urgent_close () =
  List.iter
    (fun e ->
      let t, _, broken = breaking e in
      broken := true;
      Alcotest.check_raises "the close" e (fun () -> Display.close t.display);
      Alcotest.(check bool)
        "cursor restored" true
        (Console_vte.cursor_visible t.vte))
    urgent

(* If a view raises [Sys.Break] at the close, the close raises it and shows
   the cursor again. *)
let test_urgent_beats_kept () =
  let buf, ppf, broken, size = breaking_size () in
  let view =
    View.fit (fun _ -> if !broken then raise Sys.Break else View.text "x")
  in
  let display = Display.v ~size ~now:(fun () -> 0.) ppf view in
  Display.redraw display;
  broken := true;
  Alcotest.check_raises "closing" Sys.Break (fun () -> Display.close display);
  check_buffer_restored ppf buf

(* If the terminal size raises at the close, the close raises. It first
   shows the cursor again and draws the last frame with the view. *)
let test_size_raises_at_close () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let broken = ref false in
  let size () = if !broken then raise Exit else (40, 10) in
  let display = Display.v ~size ~now:(fun () -> 0.) ppf (View.text "x") in
  Display.redraw display;
  broken := true;
  Format.pp_print_flush ppf ();
  let drawn = Buffer.length buf in
  Alcotest.check_raises "closing" Exit (fun () -> Display.close display);
  check_buffer_restored ppf buf;
  Alcotest.(check bool)
    "the view stays for the last frame" true
    (contains "x" (Buffer.sub buf drawn (Buffer.length buf - drawn)))

(* If the terminal size raises during a print, the print does not raise.
   The next redraw raises, and the view stays for the last frame. *)
let test_size_raises_in_print () =
  let buf, ppf, broken, size = breaking_size () in
  let display = Display.v ~size ~now:(fun () -> 0.) ppf (View.text "x") in
  broken := true;
  Display.print display (View.text "kept");
  Alcotest.check_raises "the redraw" Exit (fun () -> Display.redraw display);
  broken := false;
  Display.close display;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "the view stays" true
    (contains "x" (Buffer.contents buf))

(* An exception from the output that is not a write error belongs to the
   program. The print and the close raise it. The close still restores the
   formatter's own output functions. *)
let test_output_raises () =
  let failing = ref false in
  let out _ _ _ = if !failing then failwith "output" in
  let ppf = Format.make_formatter out ignore in
  let before = (Format.pp_get_formatter_out_functions ppf ()).out_string in
  let display = Display.v ~now:(fun () -> 0.) ppf (View.text "x") in
  failing := true;
  Alcotest.check_raises "the print" (Failure "output") (fun () ->
      Display.print display (View.text "kept"));
  Alcotest.check_raises "the close" (Failure "output") (fun () ->
      Display.close display);
  Alcotest.(check bool)
    "the formatter given back" true
    ((Format.pp_get_formatter_out_functions ppf ()).out_string == before);
  Display.redraw display

(* When the flush after its function raises, [with_output] still restores
   the formatter's margin and output functions. It raises the function's
   exception, unless the flush raised [Sys.Break]. *)
let test_with_output_finally () =
  let check_ending ~label ~raises ~expect =
    let failing = ref false in
    let out_flush () = if !failing then raise raises in
    let ppf = Format.make_formatter (fun _ _ _ -> ()) out_flush in
    let margin = Format.pp_get_margin ppf () in
    let display =
      Display.v
        ~size:(fun () -> (40, 10))
        ~now:(fun () -> 0.)
        ppf (View.text "x")
    in
    let before = (Format.pp_get_formatter_out_functions ppf ()).out_string in
    Alcotest.check_raises label expect (fun () ->
        Display.with_output display (fun _ ->
            failing := true;
            failwith "work"));
    Alcotest.(check int) "the margin" margin (Format.pp_get_margin ppf ());
    Alcotest.(check bool)
      "the formatter" true
      ((Format.pp_get_formatter_out_functions ppf ()).out_string == before);
    failing := false;
    try Display.close display with Exit -> ()
  in
  check_ending ~label:"the work's" ~raises:Exit ~expect:(Failure "work");
  check_ending ~label:"an interrupt" ~raises:Sys.Break ~expect:Sys.Break

(* Every function queued with [when_idle] runs, even when one raises. The
   redraw raises the first exception, or the exception the view was already
   raising (here [Out_of_memory]). *)
let test_idle_raises () =
  let queue d ran =
    Console__.Display_driver.when_idle d (fun () -> raise Exit);
    Console__.Display_driver.when_idle d (fun () -> ran := true)
  in
  let ran = ref false and armed = ref false and failure = ref None in
  let display = ref None in
  let view =
    View.fit (fun _ ->
        if !armed then begin
          armed := false;
          queue (Option.get !display) ran;
          Option.iter raise !failure
        end;
        View.text "x")
  in
  let d =
    Display.v
      ~size:(fun () -> (40, 10))
      ~now:(fun () -> 0.)
      Format.str_formatter view
  in
  display := Some d;
  armed := true;
  Alcotest.check_raises "the first raises" Exit (fun () -> Display.redraw d);
  Alcotest.(check bool) "the next one ran" true !ran;
  armed := true;
  failure := Some Out_of_memory;
  ran := false;
  Alcotest.check_raises "the worse goes on" Out_of_memory (fun () ->
      Display.redraw d);
  Alcotest.(check bool) "the next one ran" true !ran;
  armed := false;
  Display.close d

(* [Fun.protect] wraps an exception raised by its [finally]. The display
   treats the wrapped exception as the exception inside it. *)
let test_view_wrapped () =
  let wrapped = Fun.Finally_raised Sys.Break in
  let t, _, broken = breaking wrapped in
  broken := true;
  Alcotest.check_raises "the print" wrapped (fun () ->
      Display.print t.display (View.text "kept"));
  broken := false;
  close t

(* A view can close its own display. Nothing is drawn after that. *)
let test_view_closes () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let display = ref None and armed = ref false in
  let view =
    View.fit (fun _ ->
        if !armed then begin
          armed := false;
          Display.close (Option.get !display)
        end;
        View.text "x")
  in
  let d = Display.v ~size:(fun () -> (40, 10)) ~now:(fun () -> 0.) ppf view in
  display := Some d;
  armed := true;
  Display.redraw d;
  Format.pp_print_flush ppf ();
  let written = Buffer.contents buf in
  check_cursor_restored "cursor" written;
  Alcotest.(check bool)
    "no update left open" false
    (Console_vte.synchronized_open written);
  let vte = Console_vte.v ~rows:10 ~cols:40 () in
  Console_vte.feed vte written;
  Alcotest.(check (list string))
    "the last frame, once" [ "x" ]
    (List.filter (contains "x") (Console_vte.screen vte));
  Display.redraw d;
  Format.pp_print_flush ppf ();
  Alcotest.(check string) "nothing after it" written (Buffer.contents buf)

(* The code that ends a task when the display closes can raise. The display
   restores the terminal first. *)
let test_commit_raises_at_close () =
  let commit _ = raise Exit in
  let t = terminal ~styled:true ~width:40 ~height:10 ~commit () in
  ignore (Task.start t.root "running");
  Display.redraw t.display;
  Alcotest.check_raises "closing" Exit (fun () -> Display.close t.display);
  Alcotest.(check bool)
    "cursor restored" true
    (Console_vte.cursor_visible t.vte)

(* [numbered ppf] is a display on a terminal of twelve rows, with a view that
   changes at every frame. *)
let numbered ppf =
  let n = ref 0 in
  let view =
    View.fit (fun _ ->
        incr n;
        Fmt.kstr View.text "frame %d" !n)
  in
  Display.v ~size:(fun () -> (40, 12)) ~now:(fun () -> 0.) ppf view

let updates s = List.length (Astring.String.cuts ~sep:"\027[?2026h" s) - 1

(* [last_frame s] is the last frame that [numbered] drew in [s]. [rows s] is
   the rows that [s] leaves on a screen. *)
let last_frame s =
  Re.Group.get
    (List.hd (List.rev (Re.all (Re.Perl.compile_pat "frame [0-9]+") s)))
    0

let rows s =
  let vte = Console_vte.v ~rows:12 ~cols:40 () in
  Console_vte.feed vte s;
  List.filter (( <> ) "") (Console_vte.screen vte)

(* After [Sys.Break] interrupts a write, the display still knows what the
   screen holds. The next writes erase no line printed above the region, and
   the close shows the cursor. *)
let test_write_cut_short () =
  let buf = Buffer.create 256 and armed = ref false in
  let out s o l =
    if !armed then begin
      armed := false;
      raise Sys.Break
    end;
    Buffer.add_substring buf s o l
  in
  let ppf = Format.make_formatter out ignore in
  let rows = ref 1 in
  let view =
    View.fit (fun _ ->
        View.vcat (List.init !rows (fun i -> Fmt.kstr View.text "row%d" i)))
  in
  let d = Display.v ~size:(fun () -> (40, 12)) ~now:(fun () -> 0.) ppf view in
  List.iter
    (fun s -> Display.print d (View.text s))
    [ "keep-1"; "keep-2"; "keep-3"; "keep-4" ];
  rows := 4;
  armed := true;
  Alcotest.check_raises "the redraw" Sys.Break (fun () -> Display.redraw d);
  rows := 1;
  Display.print d (View.text "after");
  Display.close d;
  let vte = Console_vte.v ~rows:12 ~cols:40 () in
  Console_vte.feed vte (Buffer.contents buf);
  List.iter
    (fun keep ->
      Alcotest.(check bool)
        keep true
        (List.exists (contains keep) (Console_vte.screen vte)))
    [ "keep-1"; "keep-2"; "keep-3"; "keep-4"; "after" ];
  Alcotest.(check bool) "the cursor shown" true (Console_vte.cursor_visible vte)

(* In mode 2026 a terminal holds back what it draws until the update ends.
   So the display ends an interrupted update before it writes anything
   else. *)
let test_update_cut_in_the_middle () =
  let buf = Buffer.create 256 and armed = ref false in
  let out s o l =
    if !armed then begin
      armed := false;
      Buffer.add_substring buf s o (l / 2);
      raise Sys.Break
    end;
    Buffer.add_substring buf s o l
  in
  let ppf = Format.make_formatter out ignore in
  let d =
    Display.v
      ~size:(fun () -> (40, 12))
      ~now:(fun () -> 0.)
      ppf (View.text "row")
  in
  Display.redraw d;
  Display.print d (View.text "kept");
  armed := true;
  Alcotest.check_raises "the print" Sys.Break (fun () ->
      Display.print d (View.text "a line long enough to be cut"));
  let torn = Buffer.contents buf in
  Alcotest.(check bool)
    "cut inside the update" true
    (Console_vte.synchronized_open torn);
  Display.print d (View.text "after");
  let written = Buffer.contents buf in
  let line_after = String.index_from written (String.length torn) '\n' in
  Alcotest.(check bool)
    "ended before the next line" false
    (Console_vte.synchronized_open (String.sub written 0 line_after));
  Display.close d

(* Redraws dropped while an interrupt cuts the writes are not sent later.
   Once a write goes through, one update draws the latest frame. The frame
   drawn before the interrupt stays above the region. *)
let test_write_stays_cut () =
  let buf = Buffer.create 256 and failing = ref false in
  let out s o l =
    if !failing then raise Sys.Break;
    Buffer.add_substring buf s o l
  in
  let ppf = Format.make_formatter out ignore in
  let d = numbered ppf in
  Display.redraw d;
  let before = Buffer.length buf in
  failing := true;
  for _ = 1 to 50 do
    Alcotest.check_raises "the redraw" Sys.Break (fun () -> Display.redraw d)
  done;
  failing := false;
  Display.redraw d;
  let all = Buffer.contents buf in
  let after = String.sub all before (String.length all - before) in
  Alcotest.(check int) "one update after the cut" 1 (updates after);
  check_lines "the rows"
    [ last_frame (String.sub all 0 before); last_frame all ]
    (rows all);
  Display.close d

(* Redraws requested during a slow write, as from another fiber, do not pile
   up behind it. *)
let test_write_under_way () =
  let buf = Buffer.create 256 and during = ref ignore in
  let out s o l =
    let f = !during in
    during := ignore;
    f ();
    Buffer.add_substring buf s o l
  in
  let ppf = Format.make_formatter out ignore in
  let d = numbered ppf in
  Display.redraw d;
  let before = Buffer.length buf in
  (during :=
     fun () ->
       for _ = 1 to 50 do
         Display.redraw d
       done);
  Display.redraw d;
  let all = Buffer.contents buf in
  let after = String.sub all before (String.length all - before) in
  Alcotest.(check int) "one update" 1 (updates after);
  check_lines "the rows" [ last_frame all ] (rows all);
  Display.close d

(* An exception raised by a hook when a task ends is kept for the next call
   that reports failures. The task ends, its function's value is returned,
   and its row leaves the region. *)
let test_hook_raises () =
  let commit _ = failwith "commit" in
  let t = terminal ~styled:true ~width:40 ~height:10 ~commit () in
  Alcotest.(check int)
    "the run's value" 42
    (Task.run t.root "the task" (fun _ -> 42));
  Alcotest.check_raises "the close" (Failure "commit") (fun () ->
      Display.close t.display);
  Alcotest.(check bool)
    "no row of the task" false
    (List.exists (contains "the task") (screen t));
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  let root = Task.root ~now:(fun () -> 0.) () in
  let plain _ = failwith "plain" in
  let display = Progress.show ~progress:`Plain ~plain ppf root in
  Alcotest.(check int)
    "the run's value" 42
    (Task.run root "the task" (fun _ -> 42));
  Alcotest.check_raises "the close" (Failure "plain") (fun () ->
      Display.close display)

(* A view can raise in a frame that no call to the display requested: the
   first frame or the last. The default view then replaces it and the display
   still closes. The next redraw or the close raises the exception. *)
let test_view_exception_later () =
  let t =
    terminal ~width:40 ~height:10
      ~view:(fun _ -> View.fit (fun _ -> raise Exit))
      ()
  in
  ignore (Task.start t.root "running");
  Alcotest.check_raises "the redraw raises" Exit (fun () ->
      Display.redraw t.display);
  Alcotest.(check bool)
    "the default view drawn" true
    (List.exists (contains "running") (screen t));
  close t;
  let last root =
    View.fit (fun room ->
        match Room.height room with
        | None -> raise Exit
        | Some _ -> Progress.view root)
  in
  let t = terminal ~label:"work" ~width:40 ~height:10 ~view:last () in
  Display.redraw t.display;
  Alcotest.check_raises "the close raises" Exit (fun () -> close t);
  Alcotest.(check bool)
    "the default view printed" true
    (List.exists (contains "work") (lines t));
  Alcotest.(check bool)
    "the cursor shown" true
    (Console_vte.cursor_visible t.vte);
  Alcotest.check_raises "on a pipe" Exit (fun () ->
      ignore (plain ~view:last (fun _ _ -> ())))

(* An interrupt, or an exhausted heap or stack, in a view is not kept for a
   later call, because the view has not failed. It propagates from the call
   that was drawing, and the view stays. The close still restores the
   terminal before it propagates. *)
let test_view_urgent () =
  List.iter
    (fun e ->
      let broken = ref false in
      let view _ =
        View.fit (fun _ -> if !broken then raise e else View.text "fine")
      in
      let t = terminal ~styled:true ~width:40 ~height:10 ~view () in
      ignore (Task.start t.root "running");
      Display.redraw t.display;
      broken := true;
      Alcotest.check_raises "the print" e (fun () ->
          Display.print t.display (View.text "kept"));
      broken := false;
      Display.redraw t.display;
      Alcotest.(check bool)
        "the view stays" true
        (List.exists (contains "fine") (screen t));
      broken := true;
      Alcotest.check_raises "the close" e (fun () -> Display.close t.display);
      Alcotest.(check bool)
        "the cursor shown" true
        (Console_vte.cursor_visible t.vte))
    [ Sys.Break; Out_of_memory; Stack_overflow; Fun.Finally_raised Sys.Break ]

let suite =
  ( "display_core",
    [
      Alcotest.test_case "a view exception still allows close" `Quick
        test_view_exception_close;
      Alcotest.test_case "a view that raises later" `Quick
        test_view_exception_later;
      Alcotest.test_case "an interrupt in a view goes on" `Quick
        test_view_urgent;
      Alcotest.test_case "a task ends once" `Quick test_task_ends_once;
      Alcotest.test_case "a task's end interrupted" `Quick test_end_interrupted;
      Alcotest.test_case "urgent in a view, a print" `Quick
        test_view_urgent_print;
      Alcotest.test_case "urgent in a view, a report" `Quick
        test_view_urgent_report;
      Alcotest.test_case "urgent in a view, the close" `Quick
        test_view_urgent_close;
      Alcotest.test_case "urgent beats what was kept" `Quick
        test_urgent_beats_kept;
      Alcotest.test_case "size raises at close" `Quick test_size_raises_at_close;
      Alcotest.test_case "size raises in a print" `Quick
        test_size_raises_in_print;
      Alcotest.test_case "output raises" `Quick test_output_raises;
      Alcotest.test_case "with_output's ending" `Quick test_with_output_finally;
      Alcotest.test_case "what waits for idle raises" `Quick test_idle_raises;
      Alcotest.test_case "an exception Fun.protect wrapped" `Quick
        test_view_wrapped;
      Alcotest.test_case "a view closes its display" `Quick test_view_closes;
      Alcotest.test_case "commit raises at close" `Quick
        test_commit_raises_at_close;
      Alcotest.test_case "a hook raises as a task ends" `Quick test_hook_raises;
      Alcotest.test_case "a write cut short" `Quick test_write_cut_short;
      Alcotest.test_case "an update cut in the middle" `Quick
        test_update_cut_in_the_middle;
      Alcotest.test_case "writes that stay cut" `Quick test_write_stays_cut;
      Alcotest.test_case "a write under way" `Quick test_write_under_way;
    ] )
