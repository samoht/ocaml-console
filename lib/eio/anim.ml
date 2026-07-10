(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let line_count s =
  1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s

let run ~clock ?(ppf = Fmt.stdout) ?(fps = 10.) a f =
  if not (Terminal.is_tty ()) then f ()
  else begin
    let start = Eio.Time.now clock in
    let drawn = ref 0 in
    (* Erase the [drawn] lines of the previous frame, leaving the cursor at the
       start of where the top line was. *)
    let erase () =
      if !drawn > 0 then begin
        Fmt.pf ppf "\r\027[K";
        for _ = 2 to !drawn do
          Fmt.pf ppf "\027[1A\027[2K"
        done
      end
    in
    let draw () =
      erase ();
      let elapsed = Eio.Time.now clock -. start in
      let frame = Console.Anim.frame a ~elapsed in
      Fmt.pf ppf "%s%!" frame;
      drawn := line_count frame
    in
    let finish () =
      erase ();
      Terminal.show_cursor ppf
    in
    Terminal.hide_cursor ppf;
    let rec loop () =
      draw ();
      Eio.Time.sleep clock (1. /. fps);
      loop ()
    in
    Terminal.on_interrupt
      (fun () -> Terminal.show_cursor ppf)
      (fun () ->
        Fun.protect ~finally:finish (fun () ->
            Eio.Fiber.first (fun () -> loop ()) f))
  end
