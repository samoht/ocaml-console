(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Task-line layouts.

    A layout is a list of pieces a row renders left to right. This is the
    representation behind {!Display.Layout}. *)

(** The type for layout pieces. *)
type piece =
  | Spinner  (** The activity marker, then the outcome marker. *)
  | Label  (** The label, or the completion message. *)
  | Bar of int  (** A progress bar that many cells wide. *)
  | Percent  (** The completion percentage. *)
  | Count  (** The count pair, in the display's count cell. *)
  | Rate  (** The transfer rate of a byte-counting task. *)
  | Elapsed  (** The timer, in its {!Display_text.duration_cells}. *)
  | Text of string  (** A sanitized literal. *)
  | Accent of string  (** A sanitized literal in the accent colour. *)
  | Spacer  (** Flexible space. *)

type t
(** The type for layouts, at most one {!constructor-Label} and one
    {!constructor-Spacer}. *)

val pieces : t -> piece list
(** [pieces l] are the pieces of [l], left to right. *)

val is_label : piece -> bool
(** [is_label p] is [true] iff [p] is {!constructor-Label}. *)

val is_spacer : piece -> bool
(** [is_spacer p] is [true] iff [p] is {!constructor-Spacer}. *)

val spinner : piece
(** [spinner] is {!constructor-Spinner}. *)

val label : piece
(** [label] is {!constructor-Label}. *)

val bar : int -> piece
(** [bar n] is {!constructor-Bar} [n].

    @raise Invalid_argument if [n] is negative. *)

val percent : piece
(** [percent] is {!constructor-Percent}. *)

val count : piece
(** [count] is {!constructor-Count}. *)

val rate : piece
(** [rate] is {!constructor-Rate}. *)

val elapsed : piece
(** [elapsed] is {!constructor-Elapsed}. *)

val text : string -> piece
(** [text s] is {!constructor-Text} of [s] {!Display_text.clean}d. *)

val accent : string -> piece
(** [accent s] is {!constructor-Accent} of [s] {!Display_text.clean}d. *)

val spacer : piece
(** [spacer] is {!constructor-Spacer}. *)

val v : piece list -> t
(** [v pieces] is the layout [pieces].

    @raise Invalid_argument if [pieces] has more than one {!label} or {!spacer}.
*)

val pp : t Fmt.t
(** [pp] formats a layout for diagnostics, in an unspecified format. *)

val default : t
(** [default] is a spinner, the label, a spacer, a ten-cell bar, the count, the
    rate and the timer. *)
