(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices the widgets share: the main colour, the frame of
    tables and panels, the lines of a tree, and the spinner and progress bar of
    a {!Display}. Give the same theme to each widget to make them match. *)

(** {1:themes Themes} *)

type bar = [ `Blocky | `Smooth | `Rainbow ]
(** The type for progress bar looks: whole blocks, blocks filled to an eighth of
    a cell, or blocks in the colours of the rainbow. *)

type t
(** The type for themes. *)

val v :
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?border:Border.t ->
  ?guide:Guide.t ->
  ?spinner:Spinner.t ->
  ?bar:bar ->
  unit ->
  t
(** [v ()] is {!unicode} with the given parts replaced. [palette], the colours
    successive progress rows take in turn, defaults to [[accent]]. *)

val accent : t -> Color.t
(** [accent t] is the main colour of [t]. *)

val palette : t -> Color.t list
(** [palette t] are the colours successive progress rows take in turn. *)

val border : t -> Border.t
(** [border t] is the frame of tables and panels. *)

val guide : t -> Guide.t
(** [guide t] is the lines that connect the branches of a tree. *)

val spinner : t -> Spinner.t
(** [spinner t] is the spinner of a running task. *)

val bar : t -> bar
(** [bar t] is the look of a progress bar. *)

(** {1:predefined Predefined themes} *)

val unicode : t
(** [unicode] is cyan, with rounded frames, a braille spinner and a smooth bar.
*)

val dos : t
(** [dos] is amber and ASCII only, with a blocky bar. *)

val matrix : t
(** [matrix] is bright green, with single-line frames. *)

val rainbow : t
(** [rainbow] gives successive rows the colours of the rainbow and draws the bar
    in them. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)

(**/**)

val ok : t -> Color.t
val fail : t -> Color.t
val warn : t -> Color.t
val done_marker : t -> string
val ok_marker : t -> string
val fail_marker : t -> string
val note_marker : t -> string
val warn_marker : t -> string
val error_marker : t -> string
val committed_marker : t -> [ `Succeeded | `Failed | `Cancelled ] -> string

(**/**)
