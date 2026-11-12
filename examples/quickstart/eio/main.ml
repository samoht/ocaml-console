(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The program shown in the README's recording, on Eio. It runs test suites
   side by side as live progress rows, then prints a table of their
   results. *)

open Console

let test clock root (name, total) =
  Task.run root name @@ fun task ->
  for count = 1 to total do
    Task.set_items task ~count ~total;
    Eio.Time.Mono.sleep clock Quickstart.pause
  done

let () =
  Quickstart.main @@ fun () ->
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~label:Quickstart.label (fun root ->
      Eio.Fiber.List.iter (test clock root) Quickstart.suites)
