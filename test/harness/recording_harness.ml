(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let end_recording () =
  Env_harness.with_env
    [ ("CONSOLE_CAST", "") ]
    (fun () ->
      let unused = Format.formatter_of_buffer (Buffer.create 0) in
      ignore (Recording.start ~time:(fun () -> 0.) unused : bool))

let with_file f =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect
    ~finally:(fun () ->
      end_recording ();
      Sys.remove file)
    (fun () -> f file)
