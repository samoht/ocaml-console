(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices the widgets share: the main colour, the frame of
    tables and panels, the lines of a tree, and the spinner of a {!Display}.
    Give the same theme to each widget to make them match. *)

(** {1:themes Themes} *)

type t
(** The type for themes. *)

val v :
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?border:Border.t ->
  ?guide:Guide.t ->
  ?spinner:Spinner.t ->
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

(** {1:predefined Predefined themes} *)

val unicode : t
(** [unicode] is cyan, with rounded frames and a braille spinner. *)

val dos : t
(** [dos] is amber, with ASCII frames, tree lines and spinner. *)

val matrix : t
(** [matrix] is bright green, with single-line frames. *)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)

(**/**)

val ok : t -> Color.t
(** [ok t] is the colour of a success. *)

val fail : t -> Color.t
(** [fail t] is the colour of a failure. *)

val warn : t -> Color.t
(** [warn t] is the colour of a warning or a cancellation. *)

val done_marker : t -> string
(** [done_marker t] is the mark of a task that ended, in its row. *)

val ok_marker : t -> string
(** [ok_marker t] is the mark of a successful result. *)

val fail_marker : t -> string
(** [fail_marker t] is the mark of a failed result. *)

val note_marker : t -> string
(** [note_marker t] is the mark of an information line. *)

val warn_marker : t -> string
(** [warn_marker t] is the mark of a warning line. *)

val error_marker : t -> string
(** [error_marker t] is the mark of an error line. *)

val committed_marker : t -> [ `Succeeded | `Failed | `Cancelled ] -> string
(** [committed_marker t outcome] is the mark of a task's ending line. *)

(**/**)
