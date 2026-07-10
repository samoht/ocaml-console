(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Border styles for panels and tables.

    Provides ASCII and Unicode box-drawing character sets. *)

(** {1 Types} *)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  cross : string;  (** Intersection of horizontal and vertical lines *)
  top_cross : string;  (** T pointing down (top edge intersection) *)
  bottom_cross : string;  (** T pointing up (bottom edge intersection) *)
  left_cross : string;  (** T pointing right (left edge intersection) *)
  right_cross : string;  (** T pointing left (right edge intersection) *)
}
(** Box-drawing characters. *)

type t = { chars : chars; style : Style.t }
(** A border specification with characters and styling. *)

(** {1 Predefined borders} *)

val none : t
(** No border (empty strings). *)

val ascii : t
(** [ascii] uses "+", "-", and "|" characters. *)

val single : t
(** [single] uses Unicode single-line box-drawing characters. *)

val double : t
(** [double] uses Unicode double-line box-drawing characters. *)

val rounded : t
(** [rounded] uses Unicode rounded-corner box-drawing characters. *)

val heavy : t
(** [heavy] uses Unicode heavy-line box-drawing characters. *)

val hidden : t
(** Space characters (maintains spacing but invisible). *)

(** {1 Styling} *)

val with_style : Style.t -> t -> t
(** [with_style style border] applies a style to the border characters. *)

(** {1 Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] have the same characters and style.
*)

val pp : t Fmt.t
(** [pp ppf border] renders the border character set for diagnostics. *)

val render_char : Format.formatter -> Style.t -> string -> unit
(** [render_char ppf style char] writes [char] surrounded by [style]'s ANSI
    escape codes (or plain when [style] is {!Style.none}). Used by table and
    panel renderers to draw box-drawing characters with the border's style. *)
