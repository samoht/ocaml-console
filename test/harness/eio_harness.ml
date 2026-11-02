(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let capture_stdout progress f =
  Eio_mock.Backend.run @@ fun () ->
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    {
      saved with
      out_string = Buffer.add_substring out;
      out_flush = (fun () -> ());
    };
  Fun.protect
    ~finally:(fun () -> Format.pp_set_formatter_out_functions Fmt.stdout saved)
    (fun () ->
      Console_eio.run
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~ppf:Fmt.stdout ~progress f;
      Format.pp_print_flush Fmt.stdout ());
  Buffer.contents out
