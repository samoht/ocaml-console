(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour and UTF-8 on the standard outputs.

    [Console_tty] decides, as the OCaml command-line tools do, whether
    {!Fmt.stdout} and {!Fmt.stderr} are in colour, in how many colours, and
    whether they show UTF-8, from the terminal and the environment. A program
    calls {!setup} once, first, and then prints with [Fmt]:

    {[
    let () =
      Console_tty.setup ();
      Fmt.pr "%a@." Console.Span.pp
        (Console.Span.styled Console.Style.bold "ready")
    ]}

    This library is [console.tty]: it needs Unix, not Eio. *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Console.Theme.t ->
  unit ->
  unit
(** [setup ()] sets the style renderer, the {!Fmt.utf_8}, the
    {!Console.Color.val-depth} and the {!Console.Theme} of {!Fmt.stdout} and of
    {!Fmt.stderr}:

    - the style renderer is [style_renderer] if given, as
      [Fmt_cli.style_renderer] reads a [--color] option; otherwise it is [`None]
      if [NO_COLOR] is set and not empty, or if [TERM] is unset, empty or
      [dumb], and else [`Ansi_tty] iff the output is a terminal.
      [CLICOLOR_FORCE] and [FORCE_COLOR] are not read: [--color=always] forces
      colour;
    - {!Fmt.utf_8} is [utf_8] if given, else [true] iff the locale's character
      set is UTF-8: that of the first of [LC_ALL], [LC_CTYPE] and [LANG] that is
      set and not empty. An unset locale is not UTF-8, and the widgets print
      ASCII alone on it;
    - the colour depth is [`True_color] if [COLORTERM] is [truecolor] or [24bit]
      or [TERM] ends in [-direct], [`Ansi_256] if [TERM] contains [256color],
      and [`Ansi_16] otherwise;
    - the theme is [theme] if given, as [Cmdliner.Arg.enum Console.Theme.all]
      reads a [--theme] option; otherwise each output keeps its own. *)

val ensure : unit -> unit
(** [ensure ()] is like {!setup}[ ()] but leaves as it is each of {!Fmt.stdout}
    and {!Fmt.stderr} on which the program chose something already: a style
    renderer or {!Fmt.utf_8}, with [Fmt], [Fmt_tty] or {!setup}, or a
    {!Console.Color.val-depth}. A {!Console.Theme} is not such a choice. A
    library that prints in colour on the standard outputs calls it, so that a
    program that did not call {!setup} still gets colour and one that did keeps
    its choice; [Console_eio.run] does. *)

val is_terminal : Unix.file_descr -> bool
(** [is_terminal fd] is [true] iff [fd] is a terminal and [TERM] is set, not
    empty and not [dumb]: an output on which a program can move the cursor to
    redraw what it printed. *)
