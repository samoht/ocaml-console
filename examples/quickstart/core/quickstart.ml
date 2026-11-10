(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let suites = [ ("parser", 120); ("layout", 80); ("render", 60) ]
let pause = 0.02
let label = "Testing"

let results () =
  let ok = View.role `Succeeded (View.text "passed") in
  let row (name, n) = [ View.text name; View.text (Int.to_string n); ok ] in
  let cols =
    Table.
      [
        column (View.text "Suite");
        column ~align:`Right (View.text "Tests");
        column (View.text "Result");
      ]
  in
  Table.view cols (List.map row suites)

let theme =
  Cmdliner.Arg.(value & opt (enum Theme.all) Theme.default & info [ "theme" ])

let main run =
  let check color theme =
    Console_tty.setup ?style_renderer:color ~theme ();
    run ();
    Fmt.pr "%a@." View.pp (results ())
  in
  let cmd =
    Cmdliner.Cmd.(
      v (info "check")
        Cmdliner.Term.(const check $ Fmt_cli.style_renderer () $ theme))
  in
  exit (Cmdliner.Cmd.eval cmd)
