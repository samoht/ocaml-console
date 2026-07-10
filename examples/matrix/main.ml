(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Matrix ASCII digital-rain demo. Two showcases:

   1. a stand-alone panel whose border flows like the movie, driven by
   Console_eio.Anim.run; 2. the same rain wired in as a live banner above a progress
   display, so it animates while build steps complete.

   Run it: dune exec ocaml-console/examples/matrix/main.exe Ctrl-C stops early. *)

let banner =
  Console.Panel.lines ~padding:2
    [
      Console.Span.text "  Wake up, Neo...      ";
      Console.Span.text "  The Matrix has you.  ";
    ]

(* 1. The rain on its own, redrawn in place for a few seconds. *)
let standalone clock =
  Console_eio.Anim.run ~clock ~fps:12. (Console.Panel.rain banner) (fun () ->
      Eio.Time.sleep clock 3.0)

(* 2. The rain as a live banner over a progress display. The display's tick loop
   ([Console_eio.Display.animate]) samples the banner each frame, so it flows while
   the steps run. *)
let with_progress ~sw ~ctx clock =
  let d = Console.Display.v ~ctx ~header:"Loading the construct" () in
  Console.Display.set_banner d (Console.Panel.rain banner);
  Console_eio.Display.animate ~sw ~clock d;
  List.iter
    (fun step ->
      let r = Console.Display.row d step in
      Eio.Time.sleep clock 1.0;
      Console.Display.finish r)
    [
      "jacking in";
      "loading sparring program";
      "he's a machine";
      "free your mind";
    ];
  Console.Display.finish_all d

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Console_eio.run ~clock @@ fun ctx ->
  standalone clock;
  Eio.Switch.run (fun sw -> with_progress ~sw ~ctx clock);
  print_endline "There is no spoon."
