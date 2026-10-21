(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colours, tables, trees and progress rows for command-line tools.

    [Console] prints into the terminal's normal scrollback, as [make] or [ls]
    do, and redraws only the rows of a {!Display} while work runs; a program
    that takes over the whole screen is better served by a full-screen library.
    Windows consoles are not supported.

    A {!Span.t} is text in a {!Style.t}: colours, bold, italic and so on. The
    widgets, {!Table}, {!Tree}, {!Panel}, {!Rule} and {!Bar}, lay out spans and
    print with {!Format}, and a {!Theme} gives them one look. Every widget is a
    span too, by its [to_span], laid out when it is printed: a table goes in a
    panel, the panel in a cell of another table, beside a tree ({!Span.hcat})
    and in a display's scene, all drawn in the theme of the formatter that
    prints them. [Console_eio.run] runs a {!Display}.

    {b Guarantees.} Widths are counted in terminal cells, as the terminal draws
    the text: a wide character, such as a Chinese one or an emoji, takes two
    cells and an accent added to a letter takes none. A widget printed by its
    [pp] is no wider than the formatter's margin, and as a span it is as wide as
    its [to_span] lays it out; either ends without a line end, so ["%a@."]
    prints it on its own lines. A span holds only text that prints: a line end
    in it starts a new line and any other control character is a space, so
    caller text cannot move the cursor or change colours. Every style a printer
    turns on, it turns off before the line ends. Text cut to fit is cut between
    two characters and ends with an ellipsis. On a formatter that is not
    {!Fmt.utf_8}, the frames, lines, bars, spinners, marks and ellipses of the
    widgets are ASCII. The guarantees of a live region are those of {!Display}.
    Each guarantee and the tests that check it are listed in the repository's
    [INVARIANTS.md].

    Text is in colour when its formatter is set to colour
    ({!Fmt.set_style_renderer}), in the colours its {!Color.val-depth} allows;
    [Console_tty.setup], in the [console.tty] library, sets both for standard
    output and standard error, as the terminal and the environment say.

    {1:quickstart Quick start}

    {2:styled Styled text}

    {[
    open Console

    let demo_styled () =
      let style = Style.(bold + fg Color.green) in
      Fmt.pr "%a@." Span.pp (Span.styled style "Success!")
    ]}

    {2:tables Tables}

    {[
    open Console

    let demo_table () =
      let table =
        Table.(
          v ~border:Border.rounded
            [
              column (Span.text "Name"); column ~align:`Right (Span.text "Age");
            ]
            [
              [ Span.text "Alice"; Span.text "30" ];
              [ Span.text "Bob"; Span.text "25" ];
            ])
      in
      Fmt.pr "%a@." Table.pp table
    ]}

    {2:trees Trees}

    {[
    open Console

    let demo_tree () =
      let tree =
        Tree.v (Span.text "src")
          [
            Tree.v (Span.text "lib") [ Tree.v (Span.text "parser.ml") [] ];
            Tree.v (Span.text "test") [];
          ]
      in
      Fmt.pr "%a@." Tree.pp tree
    ]}

    {2:panels Panels}

    {[
    open Console

    let demo_panel () =
      let panel =
        Panel.v ~title:(Span.text "Status")
          (Span.text "All systems operational")
      in
      Fmt.pr "%a@." Panel.pp panel
    ]}

    {2:progress Progress}

    A program calls [Console_eio.run] once and passes the display it gets to the
    code that does the work:

    {[
    open Console

    let build_step display =
      Display.with_task display "compile" (fun step ->
          Display.set_count step ~cur:1 ~total:2;
          Display.log step "ocamlopt parser.ml";
          Display.set_count step ~cur:2 ~total:2)
    ]}

    run with [Console_eio.run ~clock ~header:"Building" build_step], [clock]
    being [Eio.Stdenv.mono_clock env]. A library can call [Console_eio.run]
    itself: inside another one, it gets the same display. *)

(** {1:modules Modules} *)

module Color = Color
(** Terminal colours, by name, by palette number or as RGB. *)

module Style = Style
(** Bold, italic, underline and colours, combined with [+]. *)

module Span = Span
(** Text in a style. *)

module Border = Border
(** Frames for tables and panels. *)

module Guide = Guide
(** The lines that connect the branches of a tree. *)

module Spinner = Spinner
(** Spinners for work of unknown length. *)

module Theme = Theme
(** One look for every widget. *)

module Bar = Bar
(** Horizontal bars, for progress and for figures. *)

module Panel = Panel
(** Text in a frame, with an optional title. *)

module Table = Table
(** Tables with headers and aligned columns. *)

module Tree = Tree
(** Trees, drawn like the output of [tree]. *)

module Rule = Rule
(** Horizontal lines with an optional label. *)

module Display = Display
(** Progress rows that are redrawn in place while work runs. *)
