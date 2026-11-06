(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The program of ../eio, on Lwt. *)

open Console
open Lwt.Syntax

let test root (name, total) =
  Console_lwt.Task.run root name @@ fun task ->
  let rec loop count =
    if count > total then Lwt.return_unit
    else begin
      Task.set_items task ~count ~total;
      let* () = Lwt_unix.sleep Quickstart.pause in
      loop (count + 1)
    end
  in
  loop 1

let () =
  Quickstart.main @@ fun () ->
  Lwt_main.run
    (Console_lwt.run ~label:Quickstart.label (fun root ->
         Lwt_list.iter_p (test root) Quickstart.suites))
