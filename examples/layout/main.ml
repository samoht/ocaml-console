(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Two progress bars laid out side by side and animated, over a row whose log
   window tails a stream of transfer lines.

   [Layout.hcat_anim] joins the two animated bar columns into one [string
   Anim.t]; it feeds a live [Display] banner, which -- like every Display redraw
   -- only repaints when the joined frame actually changes, so an idle tick
   costs nothing. The [~log_lines] row below keeps the last few log lines, so
   older ones scroll out of its window as new ones arrive.

   Run it: dune exec ocaml-console/examples/layout/main.exe *)

let width = 28
let duration = 6.0 (* seconds until the slower bar reaches 100% *)

(* One column at [elapsed]: a bold title, a determinate bar, and the percentage,
   stacked as a three-line block. [rate] is percent-per-second. *)
let column ~label ~rate ~style ~color =
  Console.Anim.v (fun ~elapsed ->
      let pct = int_of_float (Float.min 100. (elapsed *. rate)) in
      String.concat "\n"
        [
          Console.Span.to_string
            (Console.Span.styled
               Console.Style.(bold + fg Console.Color.white)
               label);
          Console.Bar.render ~width ~style ~color ~pct ();
          Fmt.str "%d%%" pct;
        ])

let view =
  Console.Layout.hcat_anim ~gutter:4
    [
      column ~label:"Download" ~rate:(100. /. 4.) ~style:`Smooth ~color:`Cyan;
      column ~label:"Verify" ~rate:(100. /. duration) ~style:`Blocky
        ~color:`Green;
    ]

(* A fake transfer-log line to feed the rotating log window. *)
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
  Console_eio.run ~clock @@ fun ctx ->
  let d = Console.Display.v ~ctx ~header:"Transfers" () in
  Console.Display.set_banner d view;
  let log = Console.Display.row ~log_lines:5 d "transfer log" in
  Eio.Switch.run (fun sw ->
      Console_eio.Display.animate ~sw ~clock d;
      for _ = 0 to int_of_float (duration /. 0.3) do
        Console.Display.log log (log_line ());
        Eio.Time.sleep clock 0.3
      done);
  Console.Display.finish log;
  Console.Display.finish_all d ~result:(`Ok "transfers complete")
