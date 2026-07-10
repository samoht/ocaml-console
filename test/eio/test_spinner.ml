(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runner animates onto a buffer (never the real terminal) so the tests are
   deterministic regardless of whether stdout is a tty. *)
module S = Console_eio.Spinner

let with_clock f =
  Eio_main.run @@ fun env ->
  let ppf = Format.formatter_of_buffer (Buffer.create 64) in
  f (Eio.Stdenv.clock env) ppf

(* [run] runs the body exactly once and returns its result. *)
let test_run_returns () =
  with_clock (fun clock ppf ->
      let calls = ref 0 in
      let r =
        S.run ~clock ~ppf ~label:"work" (fun () ->
            incr calls;
            42)
      in
      Alcotest.(check int) "returns the body's result" 42 r;
      Alcotest.(check int) "body ran exactly once" 1 !calls)

(* A failure in the body propagates out of [run] (after clearing the line). *)
let test_run_reraises () =
  with_clock (fun clock ppf ->
      Alcotest.check_raises "propagates the body's exception" (Failure "boom")
        (fun () -> ignore (S.run ~clock ~ppf (fun () -> failwith "boom"))))

let suite =
  ( "spinner",
    [
      Alcotest.test_case "run returns result" `Quick test_run_returns;
      Alcotest.test_case "run reraises" `Quick test_run_reraises;
    ] )
