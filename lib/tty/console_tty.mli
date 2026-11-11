(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour and UTF-8 on the standard outputs.

    [Console_tty] decides how {!Fmt.stdout} and {!Fmt.stderr} print, based on
    the terminal and the environment, as the OCaml command-line tools do. It
    decides whether they use colour, how many colours, and whether they show
    UTF-8. A program calls {!setup} once, at the start, then prints with [Fmt]:

    {[
    let () =
      Console_tty.setup ();
      Fmt.pr "%a@." Console.View.pp
        Console.View.(styled Console.Style.bold (text "ready"))
    ]}

    This library is [console.tty]. It needs Unix but not Eio. *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Console.Theme.t ->
  unit ->
  unit
(** [setup ()] runs {!Console.Env.setup} on {!Fmt.stdout} and {!Fmt.stderr}.
    Each one is a terminal iff [Unix.isatty] says its descriptor is one. It sets
    their style renderer, {!Fmt.utf_8} and {!Console.Color.val-depth} from
    [style_renderer], [utf_8] and the environment, with the rules of
    {!Console.Env.setup}. If [theme] is given, it becomes the program's theme
    ({!Console.Theme.set_program}). Both outputs then use it, and so do strings
    built with a new formatter, such as the one that {!Fmt.str} creates.

    When [CONSOLE_CAST] is set to a file name, [setup] records both outputs in
    that file, as [Console_eio.run] does. So a program that prints without a run
    is recorded too. Each write is recorded at time zero until a run gives the
    recording its clock. *)

val ensure : unit -> unit
(** [ensure ()] is {!Console.Env.ensure} on {!Fmt.stdout} and on {!Fmt.stderr}.
    As in {!setup}, each one is a terminal iff [Unix.isatty] says so. It is like
    [setup ()], but it does not change an output if the program already chose
    its style renderer or {!Fmt.utf_8} (with [Fmt], [Fmt_tty] or {!setup}), or
    its {!Console.Color.val-depth}. Setting a {!Console.Theme} is not such a
    choice.

    A library that prints in colour on the standard outputs calls [ensure], as
    [Console_eio.run] does. Then a program that did not call {!setup} still gets
    colour, and a program that did keeps its choice. *)

val is_terminal : Unix.file_descr -> bool
(** [is_terminal fd] is [true] iff [fd] is a terminal and [TERM] is set, not
    empty and not [dumb]. On such an output a program can move the cursor to
    redraw what it printed. *)

val size : Format.formatter -> (unit -> int * int) option
(** [size ppf] is [Some size] when [ppf] is {!Fmt.stdout} or {!Fmt.stderr} on a
    terminal ({!is_terminal}), and [None] otherwise. [size ()] is the terminal's
    width and height, read again at each call. It uses [$COLUMNS] and [$LINES]
    when they are between [1] and [4096], because POSIX lets them override the
    terminal. Otherwise it uses the size that the terminal reports, or else 80
    by 24. *)

val width : Format.formatter -> int option
(** [width ppf] is the width that output on [ppf] must fit. It is the first of:
    the width of the recorded terminal when [CONSOLE_CAST] is set; the width
    from {!size}, at most [4096]; [$COLUMNS] when it is between [1] and [4096].
    It is [None] when lines are not cut. To fit lines printed outside a run:

    {[
    let print v =
      match Console_tty.width Fmt.stdout with
      | None -> Fmt.pr "%s@." (Console.View.to_string v)
      | Some w ->
          Format.pp_set_margin Fmt.stdout w;
          Fmt.pr "%a@." Console.View.pp v
    ]} *)
