(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A step that counts its files, with a layer under it that counts bytes.
   [main.exe 2>&1 | cat] shows the plain lines a pipe gets,
   [main.exe --progress=plain] prints them on a terminal too, and
   [--progress=tty], [quiet] and [json] show the other modes. With [fail],
   the layer fails, so does the step, which a task under it does not fail
   on its own, and the program exits with 1. *)

let build ~fail clock display =
  let step = Console.Display.task ~log_lines:4 display "RUN make" in
  let layer = Console.Display.task ~parent:step display "sha256:abcd..." in
  for i = 0 to 5 do
    Console.Display.set_count step ~cur:i ~total:5;
    Fmt.kstr (Console.Display.log step) "  CC src/file%d.c" i;
    Eio.Time.Mono.sleep clock 0.2
  done;
  let total = 2147483648L in
  for i = 0 to 8 do
    Console.Display.set_bytes layer
      ~cur:(Int64.mul (Int64.of_int i) 268435456L)
      ~total;
    Eio.Time.Mono.sleep clock 0.2
  done;
  if fail then begin
    Console.Display.log layer "checksum mismatch";
    Console.Display.fail layer;
    Console.Display.fail step
  end
  else begin
    Console.Display.succeed layer;
    Console.Display.succeed step
  end;
  not fail

let main progress fail =
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  if Console_eio.run ~clock ~progress ~summary:"Building" (build ~fail clock)
  then 0
  else 1

let progress =
  Cmdliner.Arg.(
    value
    & opt (enum Console.Progress.all) `Auto
    & info [ "progress" ]
        ~doc:
          "How progress shows: $(b,auto), $(b,tty), $(b,plain), $(b,quiet) or \
           $(b,json).")

let fail = Cmdliner.Arg.(value & flag & info [ "fail" ] ~doc:"Fail the layer.")

let cmd =
  Cmdliner.Cmd.(v (info "build") Cmdliner.Term.(const main $ progress $ fail))

let () = exit (Cmdliner.Cmd.eval' cmd)
