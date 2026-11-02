(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour and UTF-8 on the standard outputs.

    [Console_tty] decides how {!Fmt.stdout} and {!Fmt.stderr} print, from the
    terminal and the environment, as the OCaml command-line tools do. It decides
    whether they are in colour, in how many colours, and whether they show
    UTF-8. A program calls {!setup} once, first, and then prints with [Fmt]:

    {[
    let () =
      Console_tty.setup ();
      Fmt.pr "%a@." Console.View.pp
        Console.View.(styled Console.Style.bold (text "ready"))
    ]}

    This library is [console.tty], which needs Unix but not Eio. *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Console.Theme.t ->
  unit ->
  unit
(** [setup ()] is {!Console.Env.setup} on {!Fmt.stdout} and on {!Fmt.stderr},
    each a terminal iff [Unix.isatty] says its descriptor is one: it sets their
    style renderer, {!Fmt.utf_8}, {!Console.Color.val-depth} and
    {!Console.Theme} from [style_renderer], [utf_8], [theme] and the
    environment, by the rules {!Console.Env.setup} states.

    [CONSOLE_CAST], set to a file name, makes [setup] record both outputs there,
    as [Console_eio.run] does, so that a program that prints without a run is
    recorded too: each write is at time zero, until a run gives the recording
    its clock. *)

val ensure : unit -> unit
(** [ensure ()] is {!Console.Env.ensure} on {!Fmt.stdout} and on {!Fmt.stderr},
    as {!setup} asks whether each is a terminal: like [setup ()], but it leaves
    alone an output on which the program chose something already. Such a choice
    is a style renderer or {!Fmt.utf_8}, set with [Fmt], [Fmt_tty] or {!setup},
    or a {!Console.Color.val-depth}. A {!Console.Theme} is not one.

    A library that prints in colour on the standard outputs calls [ensure], as
    [Console_eio.run] does. A program that did not call {!setup} still gets
    colour, and one that did keeps its choice. *)

val is_terminal : Unix.file_descr -> bool
(** [is_terminal fd] is [true] iff [fd] is a terminal and [TERM] is set, not
    empty and not [dumb]. On such an output a program can move the cursor to
    redraw what it printed. *)
