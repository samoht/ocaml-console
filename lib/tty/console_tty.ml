(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let outputs = [ (Fmt.stdout, Unix.stdout); (Fmt.stderr, Unix.stderr) ]

let setup ?style_renderer ?utf_8 ?theme () =
  Option.iter Console.Theme.set_program theme;
  List.iter
    (fun (ppf, fd) ->
      Console.Env.setup ?style_renderer ?utf_8 ~is_tty:(Unix.isatty fd) ppf)
    outputs;
  ignore (Recording.start ~time:(fun () -> 0.) Fmt.stdout : bool)

let ensure () =
  List.iter
    (fun (ppf, fd) -> Console.Env.ensure ~is_tty:(Unix.isatty fd) ppf)
    outputs

let is_terminal = Terminal.is_terminal
let size = Terminal.size
let width = Terminal.width_of
