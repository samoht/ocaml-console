(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Demonstrates Console.Display in plain mode: a 4-step job whose tasks print a
    line when they start and when they end, the last one failing with its
    output, then a summary. A fixed clock keeps the timers deterministic so cram
    diffs are stable. *)

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
  let task = Console.Display.task ~log_lines:2 display "qux.ml" in
  List.iter (Console.Display.log task) [ "line 1"; "line 2"; "line 3" ];
  now := !now +. 0.1;
  Console.Display.fail task;
  Console.Display.close display
