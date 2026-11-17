(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A case that hangs ends the run. Each case sets an alarm, whose default action
   kills the process. The alarm is far longer than any case that ends takes, so
   it never fails such a case. Windows has no alarm. *)
let guarded (name, cases) =
  if Sys.win32 then (name, cases)
  else
    ( name,
      List.map
        (fun (case, speed, f) ->
          ( case,
            speed,
            fun x ->
              ignore (Unix.alarm 300 : int);
              Fun.protect
                ~finally:(fun () -> ignore (Unix.alarm 0 : int))
                (fun () -> f x) ))
        cases )

(* Domains that Eio.Domain_manager starts on io_uring have no fallback, and in
   opam's sandboxes they run out of locked memory (ocaml-multicore/eio#958). So
   the tests run on Eio's posix backend, except on Windows, which has its own. *)
let () =
  if not Sys.win32 then begin
    Unix.putenv "EIO_BACKEND" "posix";
    Sys.set_signal Sys.sigalrm Sys.Signal_default
  end;
  Alcotest.run "console_eio"
  @@ List.map guarded
       [
         Test_display.suite;
         Test_display.report_domains_suite;
         Test_console_eio.suite;
         Test_terminal.suite;
         Test_prompt.suite;
         Test_backend.suite;
       ]
