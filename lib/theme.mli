(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices the widgets share: the main colour, the colours of
    a success, a warning and a failure, a muted colour for what frames and
    annotates, the style of headers, the marks that say how a task ended, the
    frame of tables and panels and how it moves, the lines of a tree and the
    spinner of a {!Display}. A formatter has one theme ({!set}), in which every
    widget printed on it is drawn: with ASCII glyphs alone when the formatter is
    not {!Fmt.utf_8}. Text takes a colour of that theme, such as that of a
    success, with {!Span.role}.

    {!default} reads on any background and is the one to keep when nothing is
    known of the terminal. A theme with a {!background}, such as {!dracula} or
    {!solarized_light}, is for a terminal whose own colour scheme is that one:
    its colours are chosen for that background, which the widgets do not paint,
    and on another one some of them may be hard to read. {!ascii} draws with
    ASCII alone on any terminal, and {!still} takes the motion out of any theme,
    for the users who ask for no animation. A program lets its users choose with
    a [--theme] option ({!all}).

    Colours are given as the scheme defines them, but where one would be hard to
    read; a terminal that shows fewer colours gets the nearest ones it has
    ({!Color.val-depth}). In every predefined theme, as WCAG 2.1 measures
    contrast against its {!background}, the colours of text (the {!muted}
    colour, that of the {!header} and those of the roles, which {!Span.role}
    gives text: the main colour and the colours of a success, a warning and a
    failure) are at 4.5:1 or more, and those of the progress rows at 3:1 or
    more. A theme without a background is read against black and white alike, at
    4.5:1 for its muted colour and header and 3:1 for the others: 4.5:1 on both
    asks colours as light as one another, which no amber of the 256-colour
    palette is. A frame that moves is drawn in these colours alone, so every one
    of its frames keeps to the contrast of a glyph. At 16 colours, the
    terminal's own palette draws them, which console does not know. *)

(** {1:themes Themes} *)

type role =
  [ `Accent  (** The main colour, {!accent}. *)
  | `Ok  (** The colour of a success, {!ok}. *)
  | `Warn  (** The colour of a warning, {!warn}. *)
  | `Fail  (** The colour of a failure, {!fail}. *)
  | `Muted  (** The colour of what frames and annotates, {!muted}. *)
  | `Header  (** The style of headers, {!header}. *) ]
(** The type for the parts of a theme that style text: a {!Span.role} takes one
    from the theme of the formatter it is printed on. *)

type mark =
  [ `Running  (** Work still going on, in a display's header. *)
  | `Succeeded  (** A task or a session that succeeded. *)
  | `Failed  (** A task or a session that failed. *)
  | `Cancelled  (** A task or a session that was cancelled. *)
  | `Info  (** A line of a task's output. *)
  | `Warning  (** A message that warns. *)
  | `Error  (** A message that reports an error. *)
  | `Debug  (** A message for debugging. *) ]
(** The type for the states a mark says. *)

type bar =
  [ `Line
    (** Heavy horizontal lines (U+2501), filled to the half cell, in right and
        left one eighth blocks (U+2595 and U+258F). *)
  | `Block
    (** Whole cells of full blocks (U+2588) on a light shade (U+2591), in square
        brackets, as DOS programs drew progress. *)
  | `Ascii  (** Whole cells of [#] on [-], in square brackets. *) ]
(** The type for the glyphs of a {!Bar}. *)

type motion =
  [ `Rain
    (** Digital rain: a bold head in the main colour runs round the frame, two
        ASCII glyphs flicker behind it on the frame's straight lines and the
        lines behind them fade to the muted colour. A corner or a tee keeps its
        glyph. *)
  | `Sweep
    (** The palette, graded once round the frame from its first colour at the
        top-left corner, travels round it. *)
  | `Shimmer
    (** A short highlight in the main colour travels round the frame. *) ]
(** The type for how the frame of a table or a panel moves while a live
    {!Display} draws it ([Display.set_scene]), clockwise and by the elapsed
    time. Only the colours of the border's cells change, and the glyphs of some
    to one as wide, so the frame keeps its width and its cells their place in
    every frame. Printed on a formatter, or drawn by a display without colour or
    with [NO_COLOR] set, a frame stands still in the glyphs of the theme without
    motion: a sweep keeps the colours a display starts with, and rain and a
    shimmer, a highlight that would move, keep those of the theme without
    motion. *)

type t
(** The type for themes. *)

val v :
  ?name:string ->
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?ok:Color.t ->
  ?warn:Color.t ->
  ?fail:Color.t ->
  ?muted:Color.t ->
  ?header:Style.t ->
  ?background:Color.t ->
  ?border:Border.t ->
  ?motion:motion ->
  ?guide:Guide.t ->
  ?spinner:Spinner.t ->
  ?bar:bar ->
  ?marks:(mark -> string) ->
  unit ->
  t
(** [v ()] is {!default} with the given parts replaced:

    - [name] is its {!name} (defaults to ["custom"]);
    - [accent] is the main colour (defaults to {!default}'s teal);
    - [palette] are the colours successive progress rows take in turn, and that
      a bar spreads along its length when there are several (defaults to
      [[accent]], as does [[]]);
    - [ok], [warn] and [fail] colour the marks of a success, of a warning or a
      cancellation, and of a failure or an error (default to {!default}'s green,
      amber and red);
    - [muted] colours frames, tree lines, rules, the track of a bar and the
      counts and timers of a display (defaults to {!default}'s grey);
    - [header] is the style of table headers, panel titles and a display's
      header (defaults to {!Style.bold});
    - [background] is the background the colours are chosen for, light or dark
      (defaults to none: the terminal's own). The widgets do not paint it;
    - [motion] is how frames move in a live display (defaults to none: they
      stand still);
    - [border], [guide] and [spinner] are the frame of tables and panels, the
      lines of a tree and the spinner of a running task (default to
      {!Border.rounded}, {!Guide.unicode} and {!Spinner.braille});
    - [bar] is the glyphs of bars and of a display's progress (defaults to
      [`Line]);
    - [marks] gives the mark of each state (defaults to those of {!default}). *)

val name : t -> string
(** [name t] is the name of [t]: that of {!all} for a predefined theme. *)

val accent : t -> Color.t
(** [accent t] is the main colour of [t]. *)

val palette : t -> Color.t list
(** [palette t] are the colours successive progress rows take in turn, spread
    along each bar without a style ({!Bar.v}) when there are several. It is
    never empty. *)

val ok : t -> Color.t
(** [ok t] is the colour of a success. *)

val warn : t -> Color.t
(** [warn t] is the colour of a warning and of a cancellation. *)

val fail : t -> Color.t
(** [fail t] is the colour of a failure and of an error. *)

val muted : t -> Color.t
(** [muted t] is the colour of frames, tree lines, rules, the track of a bar and
    the counts and timers of a display. *)

val header : t -> Style.t
(** [header t] is the style of table headers, panel titles and a display's
    header. *)

val background : t -> Color.t option
(** [background t] is the background the colours of [t] are chosen for, if any.
*)

val border : t -> Border.t
(** [border t] is the frame of tables and panels. *)

val motion : t -> motion option
(** [motion t] is how the frames of [t] move in a live display, if they do. *)

val guide : t -> Guide.t
(** [guide t] is the lines that connect the branches of a tree. *)

val spinner : t -> Spinner.t
(** [spinner t] is the spinner of a running task. *)

val bar : t -> bar
(** [bar t] is the glyphs of bars and of a display's progress, drawn [`Ascii] on
    a formatter that is not {!Fmt.utf_8}. *)

val mark : t -> mark -> string
(** [mark t m] is the mark of [m], on one line: a control character in it is a
    space. *)

val still : t -> t
(** [still t] is [t] with nothing that moves: no {!val-motion} and the
    {!Spinner.static} spinner, for the users who ask for no animation. *)

(** {1:predefined Predefined themes}

    The themes after {!rainbow} take their colours from a colour scheme, as its
    authors define them; their {!background} is the scheme's, their muted colour
    its grey for comments or secondary text, their headers are bold in the main
    colour unless said otherwise, and their frames [`Shimmer]. A colour below
    its contrast is moved towards the scheme's text to just above it. *)

val default : t
(** [default] is teal, with a green, an amber, a red and a grey taken from the
    xterm 256-colour palette (entries 30, 28, 136, 160 and 243) for their
    contrast on dark and light backgrounds alike, bold headers, rounded frames,
    a braille spinner and, for the states of {!type-mark} in order, a bullet, a
    check mark, a cross, a circle, a single right-pointing angle quotation mark,
    [!], a cross and a middle dot. *)

val ascii : t
(** [ascii] is {!default} drawn with ASCII characters only: ASCII frames, tree
    lines, spinner and bars, and the marks of {!dos}. Every theme is drawn with
    these on a formatter that is not {!Fmt.utf_8}; [ascii] is drawn with them on
    any. *)

val dos : t
(** [dos] is amber on black, a darker amber muted and bold amber headers, drawn
    in the box-drawing characters of code page 437 as Norton Commander drew its
    panels: {!Border.dos} frames, double lines divided by single ones, and
    {!Guide.unicode}'s single-line tree guides, with [`Block] bars, the ASCII
    spinner and the marks [[-]], [[+]], [[x]], [[~]], [#], [!], [x] and [.]. *)

val matrix : t
(** [matrix] is bright green on black, a darker green muted and bold green
    headers, with single-line frames on which digital rain falls ([`Rain]). *)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow, taken
    from the xterm 256-colour palette as {!default}'s are, spreads them along
    its bars and sweeps them round its frames ([`Sweep]). *)

val dracula : t
(** [dracula] is Dracula's purple, green, yellow and red, for its background
    [#282a36], its comment colour lightened to [#8692b7]. *)

val nord : t
(** [nord] is Nord's frost cyan, green, yellow and red, for its background
    [#2e3440], its comment grey lightened to [#969ead]. *)

val solarized_dark : t
(** [solarized_dark] is Solarized's blue, green, yellow and red, for its dark
    background [#002b36], its body text [#839496] muted and its emphasised text
    [#93a1a1] in bold for headers. *)

val solarized_light : t
(** [solarized_light] is like {!solarized_dark} for Solarized's light background
    [#fdf6e3], its green and yellow darkened to [#819400] and [#b18600], its
    comment grey darkened to [#5e7379] and its emphasised text [#586e75] in bold
    for headers. *)

val gruvbox_dark : t
(** [gruvbox_dark] is gruvbox's orange and its bright green, yellow and red, for
    its dark background [#282828], its [fg4] muted. *)

val gruvbox_light : t
(** [gruvbox_light] is gruvbox's faded orange, green, yellow and red, for its
    light background [#fbf1c7], its [fg3] muted. *)

val catppuccin_latte : t
(** [catppuccin_latte] is Catppuccin Latte's mauve, green, yellow and red, for
    its background [#eff1f5], its green and yellow darkened to [#3e9b2a] and
    [#be7919], its [subtext1] muted. *)

val catppuccin_mocha : t
(** [catppuccin_mocha] is Catppuccin Mocha's mauve, green, yellow and red, for
    its background [#1e1e2e], its [overlay2] muted. *)

val tokyo_night : t
(** [tokyo_night] is Tokyo Night's blue, green, yellow and red, for its
    background [#1a1b26], its comment colour lightened to [#7b83ab]. *)

val one_dark : t
(** [one_dark] is One Dark's accent blue, green, yellow and red, for its
    background [#282c34], its comment grey lightened to [#8e95a2] and its syntax
    blue [#61afef] in bold for headers. *)

val monokai : t
(** [monokai] is Monokai's cyan, green, yellow and pink, for its background
    [#272822], its comment colour lightened to [#939080]. *)

val github_dark : t
(** [github_dark] is GitHub Dark's accent blue, green, yellow and red, for its
    background [#0d1117], its muted foreground [#8b949e] muted. *)

val github_light : t
(** [github_light] is GitHub Light's accent blue, green, attention yellow
    [#9a6700] and red, for its background [#ffffff], its muted foreground
    [#656d76] muted. *)

(** {1:names Names} *)

val all : (string * t) list
(** [all] is every predefined theme under its {!name}, {!default} first: the
    choices of a [--theme] option, as [Cmdliner.Arg.enum all] reads them. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the theme of {!all} named [s]. The error message lists the
    names. *)

(** {1:formatters Themes of formatters} *)

val set : Format.formatter -> t -> unit
(** [set ppf t] makes [t] the theme of [ppf]: the widgets printed on [ppf] from
    now on are drawn in it. *)

val get : Format.formatter -> t
(** [get ppf] is the theme of [ppf]: the last one {!set} gave it, else
    {!default}. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)
