(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A step that counts its files, with a layer under it that counts bytes.
   [main.exe 2>&1 | cat] shows the plain lines a pipe gets. *)

let build clock display =
  let step = Console.Display.task ~log_lines:4 display "RUN make" in
  let layer = Console.Display.task ~parent:step display "sha256:abcd..." in
  for i = 0 to 5 do
    Console.Display.set_count step ~cur:i ~total:5;
    Fmt.kstr (Console.Display.log step) "  CC src/file%d.c" i;
    Eio.Time.sleep clock 0.2
  done;
  let total = 2147483648L in
  for i = 0 to 8 do
    Console.Display.set_bytes layer
      ~cur:(Int64.mul (Int64.of_int i) 268435456L)
      ~total;
    Eio.Time.sleep clock 0.2
  done;
  Console.Display.succeed layer;
  Console.Display.succeed step

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock ~header:"Building" (build clock)
