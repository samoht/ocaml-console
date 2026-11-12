(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices that the widgets share:
    - the main colour, and the colours of a success, a warning and a failure;
    - a muted colour (or faint text) for frames and annotations, and the style
      of headers;
    - the marks that show the state of a task and the level of a message;
    - the frame of tables and panels, and how it moves;
    - the lines of a tree and the spinner of a {!Display}.

    Each formatter draws in one theme: its own theme if {!set} gave it one,
    otherwise the program's theme ({!set_program}). On a formatter that is not
    {!Fmt.utf_8}, widgets use only ASCII glyphs. {!View.role} gives text a
    colour of the theme, such as the colour of a success.

    The predefined themes suit different terminals:
    - {!default} uses the terminal's own named colours and faint text, which the
      user chose for the terminal's background. It is readable wherever the
      terminal is, and it is the right choice when nothing is known about the
      terminal.
    - A theme with a {!background}, such as {!dracula} or {!solarized_light}, is
      for a terminal that uses that colour scheme. Its colours are chosen for
      that background. The widgets do not paint the background, so on another
      background some colours may be hard to read.
    - {!ascii} draws only with ASCII, on any terminal.
    - {!still} removes the motion from any theme, for users who ask for no
      animation.

    A program lets its users choose a theme with a [--theme] option ({!all}).
    Every predefined theme meets the contrast ratios given in
    {{!contrast}Contrast}. *)

(** {1:themes Themes} *)

type role =
  [ `Accent  (** The main colour, {!accent}. *)
  | `Succeeded  (** The colour of a success, {!succeeded}. *)
  | `Warning  (** The colour of a warning, {!warning}. *)
  | `Failed  (** The colour of a failure, {!failed}. *)
  | `Muted  (** The colour of frames and annotations, {!muted}. *)
  | `Header  (** The style of headers, {!header}. *) ]
(** The type for the parts of a theme that style text. {!View.role} looks one up
    in the theme of the formatter that prints the text. *)

type mark =
  [ `Running
    (** Work still going on: the line of a task when it starts on a pipe, and
        the summary until the work ends. *)
  | `Succeeded  (** A task, or the work, that succeeded. *)
  | `Failed  (** A task, or the work, that failed. *)
  | `Cancelled  (** A task, or the work, that was cancelled. *)
  | `Info  (** A line of a task's output. *)
  | `Warning  (** A message that warns. *)
  | `Error  (** A message that reports an error. *)
  | `Debug  (** A message for debugging. *) ]
(** The type for the states that a mark shows. Constructors may be added, so a
    function that gives marks must end with a case for the other states, as
    [function `Failed -> "x" | m -> mark default m] does. *)

type bar =
  [ `Line
    (** Heavy horizontal lines (U+2501), filled to the half cell, on a light
        line (U+2500), between right and left one eighth blocks (U+2595 and
        U+258F). *)
  | `Block
    (** Whole cells of full blocks (U+2588) on a light shade (U+2591), as DOS
        programs drew progress. In square brackets with [~brackets:true]. *)
  | `Ascii
    (** Whole cells of [#] on [-]. In square brackets with [~brackets:true]. *)
  ]
(** The type for the glyphs of a {!Bar}. Constructors may be added, so a match
    on a bar needs a case for the others. *)

type motion =
  [ `Rain
    (** Digital rain. A bold head in the main colour runs round the frame.
        Behind it, two ASCII glyphs flicker on the straight lines of the frame,
        and the lines behind them fade to the muted colour. Corners and tees
        keep their glyphs. *)
  | `Sweep
    (** The palette, graded once round the frame, travels round it. Its first
        colour starts at the top-left corner. *)
  | `Shimmer
    (** A short highlight in the main colour travels round the frame. *) ]
(** The type for how the frame of a table or a panel moves while a live
    {!Display} draws it in a view ([Display.set_view]). The frame moves
    clockwise, driven by the elapsed time. Only the colours of the border cells
    change, and some glyphs change to glyphs of the same width. So the frame
    keeps its width, and its cells keep their place in every frame.

    A frame stands still when it is printed once, or when a display draws it
    without colour or with [NO_COLOR] set. A sweep then keeps its colours at
    time [0.]. Rain and a shimmer are not drawn, so the frame has the colours
    and glyphs of the theme without motion.

    Constructors may be added, so a match on a motion needs a case for the
    others. *)

type t
(** The type for themes. *)

val v :
  ?name:string ->
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?succeeded:Color.t ->
  ?warning:Color.t ->
  ?failed:Color.t ->
  ?muted:Color.t ->
  ?header:Style.t ->
  ?background:Color.t ->
  ?border:Border.t ->
  ?motion:motion ->
  ?guide:Guide.t ->
  ?spinner:string list ->
  ?bar:bar ->
  ?marks:(mark -> string) ->
  unit ->
  t
(** [v ()] is {!default} with the given parts replaced:

    - [name] is its {!name} (defaults to ["custom"]);
    - [accent] is the main colour (defaults to the bright blue of {!default});
    - [palette] is the colours that successive progress rows take in turn. When
      there are several, a bar without a style spreads them along its length.
      Defaults to [[accent]]; [[]] also gives the default;
    - [succeeded] colours the mark of a success, [warning] the mark of a warning
      or a cancellation, and [failed] the mark of a failure or an error
      (defaults to the green, yellow and red of {!default});
    - [muted] colours frames, tree lines, rules, the track of a bar, and the
      counts and timers of a display (defaults to none: these are faint, in the
      terminal's own colour);
    - [header] is the style of table headers, panel titles and the name on the
      summary line of a display (defaults to {!Style.bold});
    - [background] is the light or dark background that the colours are chosen
      for (defaults to none: the terminal's own). The widgets do not paint it;
    - [motion] is how frames move in a live display (defaults to none: frames
      stand still);
    - [border] is the frame of tables and panels, [guide] the lines of a tree,
      and [spinner] the spinner of a running task (defaults to
      {!Border.rounded}, {!Guide.single} and {!Spinner.braille});
    - [bar] is the glyphs of bars and of the progress of a display (defaults to
      [`Line]);
    - [marks] gives the mark of each state (defaults to the marks of
      {!default}). States may be added ({!type-mark}), so [marks] must end with
      a case for the states that it leaves to another theme, as
      [~marks:(function `Failed -> "x" | m -> mark default m)] does.

    @raise Invalid_argument if [spinner] is empty. *)

val name : t -> string
(** [name t] is the name of [t]. For a predefined theme, it is its name in
    {!all}. *)

val accent : t -> Color.t
(** [accent t] is the main colour of [t]. *)

val palette : t -> Color.t list
(** [palette t] is the colours that successive progress rows take in turn. It is
    never empty. When there are several colours, a bar without a style
    ({!Bar.horizontal}) spreads them along its length. Otherwise the bar is in
    the single colour. *)

val succeeded : t -> Color.t
(** [succeeded t] is the colour of a success. *)

val warning : t -> Color.t
(** [warning t] is the colour of a warning and of a cancellation. *)

val failed : t -> Color.t
(** [failed t] is the colour of a failure and of an error. *)

val muted : t -> Color.t option
(** [muted t] is the colour of frames, tree lines, rules, the track of a bar,
    and the counts and timers of a display, if [t] has one. If not, these are
    faint ({!Style.faint}), in the terminal's own colour. *)

val header : t -> Style.t
(** [header t] is the style of table headers, panel titles and the name on the
    summary line of a display. *)

val background : t -> Color.t option
(** [background t] is the background that the colours of [t] are chosen for, if
    any. *)

val border : t -> Border.t
(** [border t] is the frame of tables and panels. *)

val motion : t -> motion option
(** [motion t] is how the frames of [t] move in a live display, if they move. *)

val guide : t -> Guide.t
(** [guide t] is the lines that connect the branches of a tree. *)

val spinner : t -> string list
(** [spinner t] is the frames of a {!Spinner.view} that is given no frames. The
    row of a running task shows it. It is never empty. *)

val bar : t -> bar
(** [bar t] is the glyphs of bars and of the progress of a display. On a
    formatter that is not {!Fmt.utf_8}, they are drawn as [`Ascii]. *)

val mark : t -> mark -> string
(** [mark t m] is the mark of [m], on one line. Each control character in it is
    replaced by a space. *)

val still : t -> t
(** [still t] is [t] without motion: no {!val-motion}, and a spinner of one
    frame. Use it for users who ask for no animation. *)

(** {1:predefined Predefined themes}

    The themes after {!rainbow} take their colours from a colour scheme, as its
    authors define them. Unless stated otherwise:
    - their {!background} is the background of the scheme;
    - their muted colour is the grey of the scheme for comments or secondary
      text;
    - their headers are bold, in the main colour;
    - their frames move with [`Shimmer].

    When a colour is below its contrast ({{!contrast}Contrast}), it is moved
    towards the text colour of the scheme, to just above that contrast. *)

val default : t
(** [default] uses the terminal's named colours: bright blue, with green, yellow
    and red for a success, a warning and a failure, and faint text for muted
    parts. A terminal shows them in the colours of its own scheme, which its
    user chose to be readable on its background. A recording shows them in the
    colours of the recorder's scheme. It has bold headers, rounded frames and a
    braille spinner. Its marks, for the states of {!type-mark} in order, are a
    bullet, a check mark, a cross, a circle, a single right-pointing angle
    quotation mark, [!], a cross and a middle dot. *)

val ascii : t
(** [ascii] is {!default} drawn with ASCII characters only: ASCII frames, tree
    lines, rules, spinner, bars and ellipses, and the marks of {!dos}. Every
    theme is drawn this way on a formatter that is not {!Fmt.utf_8}. [ascii] is
    drawn this way on any formatter. *)

val dos : t
(** [dos] is amber on black, with a darker amber for muted parts and bold amber
    headers. It uses the box-drawing characters of code page 437, as Norton
    Commander did for its panels:
    - {!Border.dos} frames: double lines divided by single lines;
    - the single-line tree guides of {!Guide.single};
    - [`Block] bars and the ASCII spinner;
    - the marks [[-]], [[+]], [[x]], [[~]], [#], [!], [x] and [.]. *)

val matrix : t
(** [matrix] is bright green on black, with a darker green for muted parts and
    bold green headers. Its frames are single lines with digital rain ([`Rain]).
*)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow. These
    are entries 160, 136, 28, 30, 63 and 164 of the xterm 256-colour palette,
    chosen for their contrast on both dark and light backgrounds. It also
    spreads them along its bars and sweeps them round its frames ([`Sweep]). *)

val dracula : t
(** [dracula] uses Dracula's purple, green, yellow and red, for its background
    [#282a36]. Its muted colour is Dracula's comment colour, lightened to
    [#8692b7]. *)

val nord : t
(** [nord] uses Nord's frost cyan, green, yellow and red, for its background
    [#2e3440]. Its muted colour is Nord's comment grey, lightened to [#969ead].
*)

val solarized_dark : t
(** [solarized_dark] uses Solarized's blue, green, yellow and red, for its dark
    background [#002b36]. Its muted colour is the body text [#839496], and its
    headers are the emphasised text [#93a1a1] in bold. *)

val solarized_light : t
(** [solarized_light] is like {!solarized_dark}, for Solarized's light
    background [#fdf6e3]. Its green and yellow are darkened to [#687700] and
    [#8e6b00]. Its muted colour is the comment grey, darkened to [#5e7379], and
    its headers are the emphasised text [#586e75] in bold. *)

val gruvbox_dark : t
(** [gruvbox_dark] uses gruvbox's orange and its bright green, yellow and red,
    for its dark background [#282828]. Its muted colour is [fg4]. *)

val gruvbox_light : t
(** [gruvbox_light] uses gruvbox's faded orange, green, yellow and red, for its
    light background [#fbf1c7]. Its muted colour is [fg3]. *)

val catppuccin_latte : t
(** [catppuccin_latte] uses Catppuccin Latte's mauve, green, yellow and red, for
    its background [#eff1f5]. Its green and yellow are darkened to [#327c22] and
    [#996114]. Its muted colour is [subtext1]. *)

val catppuccin_mocha : t
(** [catppuccin_mocha] uses Catppuccin Mocha's mauve, green, yellow and red, for
    its background [#1e1e2e]. Its muted colour is [overlay2]. *)

val tokyo_night : t
(** [tokyo_night] uses Tokyo Night's blue, green, yellow and red, for its
    background [#1a1b26]. Its muted colour is Tokyo Night's comment colour,
    lightened to [#7b83ab]. *)

val one_dark : t
(** [one_dark] uses One Dark's accent blue, green, yellow and red, for its
    background [#282c34]. Its muted colour is the comment grey, lightened to
    [#8e95a2], and its headers are the syntax blue [#61afef] in bold. *)

val monokai : t
(** [monokai] uses Monokai's cyan, green, yellow and pink, for its background
    [#272822]. Its muted colour is Monokai's comment colour, lightened to
    [#939080]. *)

val github_dark : t
(** [github_dark] uses GitHub Dark's accent blue, green, yellow and red, for its
    background [#0d1117]. Its muted colour is the muted foreground [#8b949e]. *)

val github_light : t
(** [github_light] uses GitHub Light's accent blue, green, attention yellow
    [#9a6700] and red, for its background [#ffffff]. Its muted colour is the
    muted foreground [#656d76]. *)

(** {1:names Names} *)

val all : (string * t) list
(** [all] is every predefined theme under its {!name}, {!default} first. These
    are the choices of a [--theme] option, read with [Cmdliner.Arg.enum all]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the theme of {!all} named [s]. The error message lists the
    names. A theme made by {!v} is not in {!all}, so [of_string (name t)] is an
    [Error] for it. *)

(** {1:formatters Themes of formatters} *)

val set_program : t -> unit
(** [set_program t] makes [t] the program's theme. From now on, every formatter
    without its own theme draws in [t]. This includes the new formatters that
    {!Fmt.str} and {!Format.asprintf} create. Call it once at startup;
    [Console_tty.setup ~theme] calls it. *)

val program : unit -> t
(** [program ()] is the last theme given to {!set_program}, or {!default}. *)

val set : Format.formatter -> t -> unit
(** [set ppf t] gives [ppf] its own theme [t]. From now on, widgets printed on
    [ppf] use [t], whatever the program's theme is. *)

val get : Format.formatter -> t
(** [get ppf] is the theme that [ppf] draws in: the last theme that {!set} gave
    it, or else the program's theme ({!program}). *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)

(** {1:contrast Contrast}

    When a terminal shows fewer colours than a theme has, the theme gets the
    nearest colours that the terminal has ({!Color.val-depth}). Contrast is
    measured as WCAG 2.1 defines it, against the {!background} of the theme. In
    every predefined theme:
    - text colours have a contrast of 4.5:1 or more. These are the {!muted}
      colour, the colour of the {!header}, and the colours that {!View.role}
      gives text (the main colour and the colours of a success, a warning and a
      failure);
    - the colours of the progress rows have a contrast of 3:1 or more.

    A theme with its own colours but no background is measured against both
    black and white: its muted colour and header at 4.5:1, its other colours at
    3:1. A moving frame is drawn only in these colours, so every step of its
    motion keeps the contrast of a still glyph.

    {!default} promises no contrast. Its colours are the terminal's named
    colours and its muted text is faint, so their contrast is the contrast of
    the terminal's own scheme. Console cannot know that scheme, and the user
    chose it. At 16 colours, every theme is drawn this way, in the named colours
    nearest to its own. *)
