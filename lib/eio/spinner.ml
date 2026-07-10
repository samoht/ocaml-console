(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let run ~clock ?(ppf = Fmt.stdout) ?(spinner = Console.Spinner.braille)
    ?(fps = 10.) ?(label = "") f =
  if not (Terminal.is_tty ()) then f ()
  else begin
    let start = Eio.Time.now clock in
    let sep = if label = "" then "" else " " in
    let draw () =
      let elapsed = Eio.Time.now clock -. start in
      Fmt.pf ppf "\r\027[K%s%s%s%!"
        (Console.Spinner.at ~fps spinner ~elapsed)
        sep label
    in
    let clear () = Fmt.pf ppf "\r\027[K%!" in
    let rec loop () =
      draw ();
      Eio.Time.sleep clock (1. /. fps);
      loop ()
    in
    Fun.protect ~finally:clear (fun () -> Eio.Fiber.first (fun () -> loop ()) f)
  end
