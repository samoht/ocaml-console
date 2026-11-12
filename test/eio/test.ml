(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A case that hangs ends the run. Each case sets an alarm, whose default action
   kills the process. The alarm is far longer than any case that ends takes, so
   it never fails such a case. *)
let guarded (name, cases) =
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

let () =
  Sys.set_signal Sys.sigalrm Sys.Signal_default;
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
