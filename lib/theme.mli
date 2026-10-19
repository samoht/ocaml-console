(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices the widgets share: the main colour, the colours of
    a success, a warning and a failure, the marks that say how a task ended, the
    frame of tables and panels, the lines of a tree and the spinner of a
    {!Display}. Give the same theme to each widget to make them match.

    Colours are given as the scheme defines them; a terminal that shows fewer
    colours gets the nearest ones it has ({!Color.val-depth}). In every
    predefined theme, the main colour, the colours of a success, a warning and a
    failure and those of the progress rows have a contrast of 3:1 or more with
    its {!background}, as WCAG 2.1 measures it, or with black and with white
    alike when it has none. *)

(** {1:themes Themes} *)

type mark =
  [ `Running  (** Work still going on, in a display's header. *)
  | `Succeeded  (** A task or a session that succeeded. *)
  | `Failed  (** A task or a session that failed. *)
  | `Cancelled  (** A task or a session that was cancelled. *)
  | `Info  (** A log line that informs. *)
  | `Warning  (** A log line that warns. *)
  | `Error  (** A log line that reports an error. *) ]
(** The type for the states a mark says. *)

type t
(** The type for themes. *)

val v :
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?ok:Color.t ->
  ?warn:Color.t ->
  ?fail:Color.t ->
  ?header:Style.t ->
  ?background:Color.t ->
  ?border:Border.t ->
  ?guide:Guide.t ->
  ?spinner:Spinner.t ->
  ?marks:(mark -> string) ->
  unit ->
  t
(** [v ()] is {!unicode} with the given parts replaced:

    - [accent] is the main colour (defaults to {!unicode}'s teal);
    - [palette] are the colours successive progress rows take in turn (defaults
      to [[accent]], as does [[]]);
    - [ok], [warn] and [fail] colour the marks of a success, of a warning or a
      cancellation, and of a failure or an error (default to {!unicode}'s green,
      amber and red);
    - [header] is the style of a display's header (defaults to {!Style.bold});
    - [background] is the background the colours are chosen for, light or dark
      (defaults to none: the terminal's own). The widgets do not paint it;
    - [border], [guide] and [spinner] are the frame of tables and panels, the
      lines of a tree and the spinner of a running task (default to
      {!Border.rounded}, {!Guide.unicode} and {!Spinner.braille});
    - [marks] gives the mark of each state (defaults to those of {!unicode}). *)

val accent : t -> Color.t
(** [accent t] is the main colour of [t]. *)

val palette : t -> Color.t list
(** [palette t] are the colours successive progress rows take in turn. It is
    never empty. *)

val ok : t -> Color.t
(** [ok t] is the colour of a success. *)

val warn : t -> Color.t
(** [warn t] is the colour of a warning and of a cancellation. *)

val fail : t -> Color.t
(** [fail t] is the colour of a failure and of an error. *)

val header : t -> Style.t
(** [header t] is the style of a display's header. *)

val background : t -> Color.t option
(** [background t] is the background the colours of [t] are chosen for, if any.
*)

val border : t -> Border.t
(** [border t] is the frame of tables and panels. *)

val guide : t -> Guide.t
(** [guide t] is the lines that connect the branches of a tree. *)

val spinner : t -> Spinner.t
(** [spinner t] is the spinner of a running task. *)

val mark : t -> mark -> string
(** [mark t m] is the mark of [m], on one line: a control character in it is a
    space. *)

(** {1:predefined Predefined themes}

    The themes after {!rainbow} take their colours from a colour scheme, as its
    authors define them; their {!background} is the scheme's. *)

val unicode : t
(** [unicode] is teal, with a green, an amber and a red taken from the xterm
    256-colour palette (entries 30, 28, 136 and 160) for their contrast on dark
    and light backgrounds alike, with rounded frames, a braille spinner and, for
    the states of {!type-mark} in order, a bullet, a check mark, a cross, a
    circle, a single right-pointing angle quotation mark, [!] and a cross. *)

val ascii : t
(** [ascii] is {!unicode} drawn with ASCII characters only: ASCII frames, tree
    lines and spinner, and the marks of {!dos}. It is the look for a terminal
    whose character set is not UTF-8. *)

val dos : t
(** [dos] is amber on black, with ASCII frames, tree lines and spinner and the
    marks [[-]], [[+]], [[x]], [[~]], [#], [!] and [x]. *)

val matrix : t
(** [matrix] is bright green on black, with single-line frames. *)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow, taken
    from the xterm 256-colour palette as {!unicode}'s are. *)

val dracula : t
(** [dracula] is Dracula's purple, green, yellow and red, for its background
    [#282a36]. *)

val nord : t
(** [nord] is Nord's frost cyan, green, yellow and red, for its background
    [#2e3440]. *)

val solarized_dark : t
(** [solarized_dark] is Solarized's blue, green, yellow and red, for its dark
    background [#002b36]. *)

val solarized_light : t
(** [solarized_light] is like {!solarized_dark} for Solarized's light background
    [#fdf6e3], its green and yellow darkened to [#819400] and [#b18600] for
    contrast. *)

val gruvbox_dark : t
(** [gruvbox_dark] is gruvbox's orange and its bright green, yellow and red, for
    its dark background [#282828]. *)

val gruvbox_light : t
(** [gruvbox_light] is gruvbox's faded orange, green, yellow and red, for its
    light background [#fbf1c7]. *)

val catppuccin_latte : t
(** [catppuccin_latte] is Catppuccin Latte's mauve, green, yellow and red, for
    its background [#eff1f5], its green and yellow darkened to [#3e9b2a] and
    [#be7919] for contrast. *)

val catppuccin_mocha : t
(** [catppuccin_mocha] is Catppuccin Mocha's mauve, green, yellow and red, for
    its background [#1e1e2e]. *)

val tokyo_night : t
(** [tokyo_night] is Tokyo Night's blue, green, yellow and red, for its
    background [#1a1b26]. *)

val one_dark : t
(** [one_dark] is One Dark's accent blue, green, yellow and red, for its
    background [#282c34]. *)

val monokai : t
(** [monokai] is Monokai's cyan, green, yellow and pink, for its background
    [#272822]. *)

val github_dark : t
(** [github_dark] is GitHub Dark's accent blue, green, yellow and red, for its
    background [#0d1117]. *)

val github_light : t
(** [github_light] is GitHub Light's accent blue, green, yellow and red, for its
    background [#ffffff]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)
