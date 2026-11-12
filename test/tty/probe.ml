(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* This program calls [Console_tty.ensure], then prints the renderer and the
   depth of standard output. Before that, [chosen] sets a colour renderer on
   standard output, and [themed] sets only a theme. *)

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
