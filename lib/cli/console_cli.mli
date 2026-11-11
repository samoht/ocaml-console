(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line options for the look of a program's output.

    {!setup} adds [--color] and [--theme] options to a Cmdliner program. It
    applies them to both standard outputs and sets the program's theme. The
    progress rows on standard error, the text on standard output and the strings
    built with {!Fmt.str} all use them:

    {[
    let main () =
      Fmt.pr "%a@." Console.View.pp
        (Console.Panel.view (Console.View.text "hello"))

    let cmd =
      Cmdliner.Cmd.v
        (Cmdliner.Cmd.info "hello")
        Cmdliner.Term.(const main $ Console_cli.setup ())
    ]}

    This library is [console.cli]. It needs Cmdliner and Unix. *)

val setup : ?docs:string -> unit -> unit Cmdliner.Term.t
(** [setup ()] is a term that reads two options and passes them to
    [Console_tty.setup ?style_renderer ?theme ()]:
    - [--color=auto|always|never], read by [Fmt_cli.style_renderer];
    - [--theme=NAME], where [NAME] is a name in {!Console.Theme.all}.

    Without [--color], the environment decides colour. Without [--theme], the
    program's theme ({!Console.Theme.program}) does not change. The manual lists
    the options in the section [docs] (defaults to
    [Cmdliner.Manpage.s_common_options]). Do not also use
    [Fmt_cli.style_renderer]: it defines the same [--color] option. *)
