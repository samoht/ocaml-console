(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The screens of a {!Console.Progress} session on a terminal. The control
    sequences go through Matrix's terminal emulator, and the screen is printed
    after each frame, so a cram test pins what a terminal shows: nested rows
    with log windows, rows that end, new log lines, and the redraws between
    them.

    A fixed clock keeps spinners and timers deterministic. Width and height come
    from [$COLUMNS] and [$LINES], which the cram test sets, so the wrapping and
    the height cap are stable. *)

let geom name default =
  match Option.bind (Sys.getenv_opt name) int_of_string_opt with
  | Some v -> v
  | None -> default

let () =
  let height = geom "LINES" 24 in
  let width = geom "COLUMNS" 80 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let sync () =
    Format.pp_print_flush ppf ();
    Console_vte.feed vte (Buffer.contents buf);
    Buffer.clear buf
  in
  (* [frame title] redraws the display, as the driver's tick does, then prints
     the screen. On its own the display repaints at a pace that this pinned
     clock never reaches, so without the redraw a frame would show the region
     as the previous one left it. *)
  let frame ?display title =
    Option.iter Console.Display.redraw display;
    sync ();
    Fmt.pr "--- %s ---@." title;
    List.iter
      (fun l -> Fmt.pr "%s@." l)
      (Console_vte.screen ~scrollback:true vte)
  in
  let module P = Console.Task in
  let root = P.root ~label:"Build" ~now:(fun () -> !now) () in
  let display =
    Console.Progress.show ~size:(fun () -> (width, height)) ppf root
  in
  let frame title = frame ~display title in
  (* A tree: app -> {resolve, compile}, each with a small log window. *)
  let app = P.start ~log_lines:2 root "app" in
  P.log app "planning";
  frame "tree with one node + a log";
  let resolve = P.start ~log_lines:2 app "resolve" in
  P.log resolve "fetch index";
  let compile = P.start ~log_lines:3 app "compile" in
  P.log compile "cc a.o";
  P.log compile "cc b.o";
  frame "two children, each streaming logs";
  P.succeed resolve;
  frame "resolve finished, under its parent, siblings still live";
  P.log compile "cc c.o";
  P.log compile "link";
  P.log app "still working";
  frame "more logs after the row closed";
  P.succeed compile;
  P.succeed app;
  P.succeed root;
  Console.Display.close display;
  sync ();
  Fmt.pr "--- all finished ---@.";
  List.iter (fun l -> Fmt.pr "%s@." l) (Console_vte.screen ~scrollback:true vte)
