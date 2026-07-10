(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* In the test harness stdout is not a tty, so [run] suppresses the animation
   and runs the body directly -- deterministic regardless of environment. *)

let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.clock env)
let anim = Console.Panel.rain (Console.Panel.lines [ Console.Span.text "neo" ])

let test_run_returns () =
  with_clock (fun clock ->
      let calls = ref 0 in
      let r =
        Console_eio.Anim.run ~clock anim (fun () ->
            incr calls;
            42)
      in
      Alcotest.(check int) "returns the body's result" 42 r;
      Alcotest.(check int) "body ran exactly once" 1 !calls)

let test_run_reraises () =
  with_clock (fun clock ->
      Alcotest.check_raises "propagates the body's exception" (Failure "boom")
        (fun () ->
          ignore (Console_eio.Anim.run ~clock anim (fun () -> failwith "boom"))))

let suite =
  ( "anim",
    [
      Alcotest.test_case "run returns result" `Quick test_run_returns;
      Alcotest.test_case "run reraises" `Quick test_run_reraises;
    ] )
