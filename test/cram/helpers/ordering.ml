(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A display on standard error, while the program prints its results on
    standard output without flushing it. *)

let () =
  let display = Console.Display.v ~now:(fun () -> 0.) Fmt.stderr in
  Fmt.pr "result 1\n";
  let task = Console.Display.task display "fetch" in
  Fmt.pr "result 2\n";
  Console.Display.log task "fetched";
  Console.Display.succeed task;
  Fmt.pr "result 3\n";
  Console.Display.close display
