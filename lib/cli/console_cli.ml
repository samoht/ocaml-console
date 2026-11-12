(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let theme ~docs =
  let doc =
    Fmt.str "The look of the output: %s."
      (Cmdliner.Arg.doc_alts_enum Console.Theme.all)
  in
  Cmdliner.Arg.(
    value
    & opt (some (enum Console.Theme.all)) None
    & info [ "theme" ] ~docs ~docv:"NAME" ~doc)

let setup ?(docs = Cmdliner.Manpage.s_common_options) () =
  let setup style_renderer theme =
    Console_tty.setup ?style_renderer ?theme ()
  in
  Cmdliner.Term.(const setup $ Fmt_cli.style_renderer ~docs () $ theme ~docs)
