(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal styling and layout for command-line tools.

    A {!Span.t} is the unit of output: a run of text carrying a {!Style.t} -- a
    {!Color.t}, bold, italic, and so on. {!Span.width} measures spans the way a
    terminal does, counting wide CJK and emoji cells rather than bytes, so the
    widgets align correctly. {!Panel}, {!Table}, {!Tree} and {!Rule} compose
    spans into bordered, columned, nested and divided blocks; {!Border} and
    {!Guide} hold their drawing vocabulary, {!Layout} joins blocks, and a
    {!Theme} gives the whole set one consistent look. {!Display} is the live,
    in-place progress view for long-running work.

    {1:geometry Terminal geometry contract}

    Widths are terminal-cell widths, never byte or code-point counts. Whenever a
    widget receives a width--directly, through a formatter margin, or from a
    {!Display}--that width is a hard ceiling for every physical row. ANSI
    escapes occupy no cells; wide and combining glyphs follow terminal width;
    malformed UTF-8 is rendered as a replacement glyph; terminal controls in
    content cannot move the cursor. Fitting preferences may change which content
    is shortened, but may not push a border into the next row.

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
        Tree.v
          (Tree.Node
             ( Span.text "src",
               [
                 Tree.Node
                   (Span.text "lib", [ Tree.Node (Span.text "parser.ml", []) ]);
                 Tree.Node (Span.text "test", []);
               ] ))
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

    A {!Display} keeps permanent event history separate from an immutable view
    of current activity. On a terminal the activity view is redrawn in one
    bounded region; finished tasks and logs remain in scrollback. Without a
    terminal, only the permanent history is emitted.

    A top-level binary calls [Console_eio.run] once and passes the resulting
    display to its libraries. Ordinary widgets are regular pretty-printers; only
    semantic task and event operations require the display. A nested
    [Console_eio.run] joins the existing session, so independently composable
    libraries cannot claim the terminal twice.

    {[
    open Console

    let build_step display =
      let step = Display.task display "compile" in
      Display.set_count step ~cur:1 ~total:2;
      Display.log step "compiled parser.ml";
      Display.succeed step
    ]}

    The binary runs it with
    [Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building" build_step].

    {2:animation Animation}

    An {!Anim.t} is a value sampled at an elapsed time. {!Spinner}, {!Bar},
    {!Panel} and {!Table} expose one.

    {[
    open Console

    let demo_anim () =
      let bar = Bar.anim ~style:`Smooth ~width:20 () in
      Fmt.pr "%s@." (Anim.frame bar ~elapsed:0.5)
    ]} *)

(** {1:setup Colour on or off} *)

val style_renderer :
  ?renderer:Fmt.style_renderer ->
  getenv:(string -> string option) ->
  is_tty:bool ->
  unit ->
  Fmt.style_renderer
(** [style_renderer ~renderer ~getenv ~is_tty ()] is whether an output takes
    ANSI styling, read from the environment [getenv] reads:
    - [renderer] when given, the reader's [--color=always|never];
    - [`None] when [NO_COLOR] is set to a non-empty value
      ({{:https://no-color.org}no-color.org});
    - [`None] when [TERM] is unset, empty or [dumb];
    - [`Ansi_tty] iff [is_tty], the output being a terminal, otherwise.

    [Console_eio.setup] sets stdout and stderr by it, and [Console_eio.run] the
    formatter it draws on when nobody else did. *)

(** {1:modules Modules} *)

module Color = Color
(** Named and 24-bit terminal colours. *)

module Style = Style
(** Text attributes: foreground and background colour, bold, italic, underline.
*)

module Gradient = Gradient
(** Linear colour gradients laid over terminal cells. *)

module Span = Span
(** A run of styled text: the unit every widget lays out. *)

module Border = Border
(** Box-drawing border presets -- ASCII, single, rounded, and friends. *)

module Guide = Guide
(** Shared guide glyphs for trees and nested progress rows. *)

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

module Rule = Rule
(** Horizontal rules with optional styled labels. *)

module Layout = Layout
(** Joining rendered text blocks side by side into aligned columns. *)

module Display = Display
(** A live progress display that redraws rows in place. *)

module Input = Input
(** Cooked terminal input: a line editor with history and tab completion. *)
