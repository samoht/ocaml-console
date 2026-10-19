(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* A wrapper hands [run] a context and then has to write to the same surface
   itself: the newline that closes a line the session left open is written
   after the display is torn down, so it cannot come out of the display. The
   context is where that surface was named, and [ctx_ppf] is where the wrapper
   reads it, so the byte it writes lands in the session's own transcript and
   after it. *)
let test_a_context_names_the_surface_it_draws_on () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let ctx = ctx ~ppf ~now () in
  Display.run ~ctx ~mode:`History ~header:"Building" (fun display ->
      let resolve = Display.task display "resolve" in
      advance 1.3;
      Display.succeed resolve);
  Fmt.pf (Display.ctx_ppf ctx) "after the session@.";
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "the wrapper wrote onto the session's own surface, after it"
    ("=> resolve  1.3s\n" ^ "[+] Building 1.3s (1/1)\n" ^ "after the session\n")
    (Buffer.contents buffer)

let suite =
  ( "display_ctx",
    [
      Alcotest.test_case "a context names the surface it draws on" `Quick
        test_a_context_names_the_surface_it_draws_on;
    ] )
