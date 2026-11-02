(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)
