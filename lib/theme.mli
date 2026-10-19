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
    colours gets the nearest ones it has ({!Color.depth}). *)

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

    - [accent] is the main colour (defaults to {!Color.cyan});
    - [palette] are the colours successive progress rows take in turn (defaults
      to [[accent]], as does [[]]);
    - [ok], [warn] and [fail] colour the marks of a success, of a warning or a
      cancellation, and of a failure or an error (default to {!Color.green},
      {!Color.yellow} and {!Color.red});
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

(** {1:predefined Predefined themes} *)

val unicode : t
(** [unicode] is cyan, with rounded frames, a braille spinner and, for the
    states of {!type-mark} in order, a bullet, a check mark, a cross, a circle,
    a single right-pointing angle quotation mark, [!] and a cross. *)

val dos : t
(** [dos] is amber on black, with ASCII frames, tree lines and spinner and the
    marks [[-]], [[+]], [[x]], [[~]], [#], [!] and [x]. *)

val matrix : t
(** [matrix] is bright green on black, with single-line frames. *)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)
