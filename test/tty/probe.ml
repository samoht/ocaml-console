(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A program that calls [Console_tty.ensure] and prints the renderer and depth
   standard output ends up with. Before that, with [chosen] it sets a colour
   renderer on standard output, and with [themed] it gives it a theme only. *)

let () =
  (match Sys.argv with
  | [| _; "chosen" |] -> Fmt.set_style_renderer Fmt.stdout `Ansi_tty
  | [| _; "themed" |] -> Console.Theme.set Fmt.stdout Console.Theme.nord
  | _ -> ());
  Console_tty.ensure ();
  let renderer =
    match Fmt.style_renderer Fmt.stdout with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let depth =
    match Console.Color.depth Fmt.stdout with
    | `Ansi_16 -> "16"
    | `Ansi_256 -> "256"
    | `True_color -> "24"
  in
  print_string (String.concat " " [ renderer; depth ])
