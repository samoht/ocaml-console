(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The program the README opens with: test suites run as live progress rows,
   then a table of their results, in the theme of --theme and the colours of
   --color. *)

(* $MDX part-begin=main *)
open Console

let suites = [ ("parser", 120); ("layout", 80); ("render", 60) ]

let test clock display (name, total) =
  Display.with_task display name @@ fun task ->
  for cur = 1 to total do
    Display.set_count task ~cur ~total;
    Eio.Time.Mono.sleep clock 0.02
  done

let main color theme =
  Console_tty.setup ?style_renderer:color ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  Console_eio.run ~clock ~header:"Testing" (fun d ->
      Eio.Fiber.List.iter (test clock d) suites);
  let ok = Span.role `Ok "passed" in
  let row (name, n) = [ Span.text name; Span.text (Int.to_string n); ok ] in
  let cols =
    Table.
      [
        column (Span.text "Suite");
        column ~align:`Right (Span.text "Tests");
        column (Span.text "Result");
      ]
  in
  Fmt.pr "%a@." Table.pp (Table.v cols (List.map row suites))

let theme =
  Cmdliner.Arg.(value & opt (enum Theme.all) Theme.default & info [ "theme" ])

let cmd =
  Cmdliner.Cmd.(
    v (info "check")
      Cmdliner.Term.(const main $ Fmt_cli.style_renderer () $ theme))

let () = exit (Cmdliner.Cmd.eval cmd)
(* $MDX part-end *)
