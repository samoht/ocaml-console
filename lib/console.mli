(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colours, tables, trees and progress rows for command-line tools.

    A {!Span.t} is a piece of text in a {!Style.t}: colours, bold, italic and so
    on. The widgets, {!Table}, {!Tree}, {!Panel} and {!Rule}, lay out spans and
    print with {!Format}, and a {!Theme} gives them one look. A {!Display} shows
    progress rows that are redrawn in place while work runs; [Console_eio.run]
    starts one.

    Widths are counted in terminal cells, as the terminal draws the text: a wide
    character, such as a Chinese one or an emoji, takes two cells and an accent
    added to a letter takes none. A widget printed on a formatter is no wider
    than the formatter's margin. A control character in a span, which could move
    the cursor or change the colours, is printed as a space.

    Text is in colour when its formatter is set to colour
    ({!Fmt.set_style_renderer}); [Console_eio.setup] sets standard output and
    standard error as the terminal and the environment say.

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
          of_rows ~border:Border.rounded
            [ column "Name"; column ~align:`Right "Age" ]
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
      let step = Display.task display "compile" in
      Display.set_count step ~cur:1 ~total:2;
      Display.log step "compiled parser.ml";
      Display.succeed step
    ]}

    run with [Console_eio.run ~clock ~header:"Building" build_step]. A library
    can call [Console_eio.run] itself: inside another one, it gets the same
    display. *)

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

module Input = Input
(** A line editor, with history and completion. *)
