(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal styling and layout for command-line tools.

    A {!Span.t} is the unit of output: a run of text carrying a {!Style.t} -- a
    {!Color.t}, bold, italic, and so on. {!Width} measures spans the way a
    terminal does, counting wide CJK and emoji cells rather than bytes, so the
    widgets align correctly. {!Panel}, {!Table} and {!Tree} compose spans into
    bordered, columned and nested blocks; a {!Border} draws their frame and a
    {!Theme} gives a whole set of widgets one consistent look. {!Display} is the
    live, in-place progress view for long-running work.

    {1 Quick start}

    {2 Styled text}

    {[
    open Console

    let demo_styled () =
      let style = Style.(bold + fg Color.green) in
      Fmt.pr "%a@." (Style.styled style Fmt.string) "Success!"
    ]}

    {2 Tables}

    {[
    open Console

    let demo_table ~ctx =
      let table =
        Table.(
          of_rows ~border:Border.rounded
            [ column "Name"; column ~align:`Right "Age" ]
            [
              [ Span.text "Alice"; Span.text "30" ];
              [ Span.text "Bob"; Span.text "25" ];
            ])
      in
      Fmt.pr "%a@." (Table.pp ~ctx) table
    ]}

    {2 Trees}

    {[
    open Console

    let demo_tree () =
      let tree =
        Tree.of_tree
          (Tree.Node
             ( Span.text "src",
               [
                 Tree.Node
                   (Span.text "lib", [ Tree.Node (Span.text "tty.ml", []) ]);
                 Tree.Node (Span.text "test", []);
               ] ))
      in
      Fmt.pr "%a@." Tree.pp tree
    ]}

    {2 Panels}

    {[
    open Console

    let demo_panel () =
      let panel =
        Panel.v ~title:(Span.text "Status")
          (Span.text "All systems operational")
      in
      Fmt.pr "%a@." Panel.pp panel
    ]}

    {2 Progress}

    A {!Display} owns a live region of rows that update in place; with no
    terminal it degrades to one tidy line per finished step. A {!Theme} fixes
    the look -- spinner, bar style, markers -- across the whole display.

    A display draws on a {!Display.ctx} (where to write, the clock, the terminal
    geometry). The convention: a library takes the context as a [~ctx] argument
    and threads it down; the top-level binary calls [Console_eio.run] once to
    build one from the eio environment and passes it in.

    {[
    open Console

    let build_step ~ctx =
      let d = Display.v ~ctx ~theme:Theme.unicode ~header:"Building" () in
      let step = Display.row d "compile" in
      Display.set_count step ~cur:1 ~total:2;
      Display.finish step;
      Display.finish_all d
    ]}

    {2 Animation}

    An {!Anim.t} is a value sampled at an elapsed time. {!Spinner}, {!Bar} and
    {!Panel.rain} expose one; {!Display.set_banner} (or [Console_eio.Anim.run])
    drives it on a tick.

    {[
    open Console

    let demo_anim () =
      let bar = Bar.anim ~style:`Smooth ~width:20 () in
      Fmt.pr "%s@." (Anim.frame bar ~elapsed:0.5)
    ]} *)

(** {1 Modules} *)

module Color = Color
(** Named and 24-bit terminal colours. *)

module Style = Style
(** Text attributes: foreground and background colour, bold, italic, underline.
*)

module Width = Width
(** The display width of text in terminal cells, counting wide glyphs as two. *)

module Span = Span
(** A run of styled text: the unit every widget lays out. *)

module Border = Border
(** Box-drawing border presets -- ASCII, single, rounded, and friends. *)

module Anim = Anim
(** Time-varying values: the shape every animatable component shares. *)

module Spinner = Spinner
(** Animated spinners for indeterminate work. *)

module Theme = Theme
(** A shared visual theme -- border, tree guide, colours and live-display glyphs
    -- that styles every widget consistently. *)

module Bar = Bar
(** Horizontal progress bars, determinate or moving. *)

module Panel = Panel
(** A bordered box around content, with an optional title and subtitle. *)

module Table = Table
(** Columned tables with per-column alignment, wrapping and borders. *)

module Tree = Tree
(** Nested trees drawn with guide lines. *)

module Layout = Layout
(** Joining rendered text blocks side by side into aligned columns. *)

module Display = Display
(** A live progress display that redraws rows in place. *)

module Input = Input
(** Cooked terminal input: a line editor with history and tab completion. *)
