(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Matrix-themed permanent history plus a bounded view of current activity.

   Run it: dune exec ocaml-console/examples/matrix/main.exe Ctrl-C stops early. *)

let with_progress clock =
  Console_eio.run ~clock ~theme:Console.Theme.matrix
    ~header:"Loading the construct" (fun display ->
      List.iter
        (fun step ->
          let task = Console.Display.task display step in
          Console.Display.log task "entered the construct";
          Eio.Time.sleep clock 1.0;
          Console.Display.succeed task)
        [
          "jacking in";
          "loading sparring program";
          "he's a machine";
          "free your mind";
        ];
      Console.Display.set_result display (`Ok "There is no spoon."))

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  with_progress clock
