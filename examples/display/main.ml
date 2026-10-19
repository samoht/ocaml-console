(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A durable transfer history with a bounded semantic activity view. Finished
   tasks and every log line stay in scrollback; only the two running transfers
   are redrawn in place. *)

let duration = 6.0
let actions = [| "GET"; "HEAD"; "fetch"; "verify"; "extract" |]

let log_line () =
  Fmt.str "%-7s sha256:%06x  layer %d/8  %4d KiB"
    actions.(Random.int (Array.length actions))
    (Random.int 0x1000000)
    (1 + Random.int 8)
    (Random.int 4096)

let () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock ~header:"Transfers" (fun display ->
      let total = Int64.mul 64L (Int64.mul 1024L 1024L) in
      let download = Console.Display.task ~log_lines:3 display "download" in
      let verify = Console.Display.task ~log_lines:2 display "verify" in
      for step = 0 to 20 do
        let fraction = float step /. 20. in
        let downloaded = Int64.of_float (fraction *. Int64.to_float total) in
        let verified =
          Int64.of_float (Float.min 1. (fraction *. 0.7) *. Int64.to_float total)
        in
        Console.Display.set_bytes download ~cur:downloaded ~total;
        Console.Display.set_bytes verify ~cur:verified ~total;
        Console.Display.log download (log_line ());
        Eio.Time.sleep clock (duration /. 20.)
      done;
      Console.Display.set_bytes verify ~cur:total ~total;
      Console.Display.succeed download;
      Console.Display.succeed verify;
      Console.Display.set_result display (`Ok "transfers complete"))
