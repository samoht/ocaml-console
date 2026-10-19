(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    A style is a set of attributes and colours, composed with {!( + )}:
    [Style.(bold + fg Color.red + underline)]. A colour may be a {!Gradient.t},
    laid over the cells the style draws. *)

(** {1:styles Styles} *)

type t
(** The type for styles. *)

val none : t
(** [none] is the empty style. *)

val bold : t
(** [bold] is the style of bold text. *)

val faint : t
(** [faint] is the style of faint text. *)

val italic : t
(** [italic] is the style of italic text. *)

val underline : t
(** [underline] is the style of underlined text. *)

val blink : t
(** [blink] is the style of blinking text. *)

val reverse : t
(** [reverse] is the style of reverse video text. *)

val strikethrough : t
(** [strikethrough] is the style of struck-through text. *)

val fg : Color.t -> t
(** [fg c] is the style with foreground colour [c]. *)

val bg : Color.t -> t
(** [bg c] is the style with background colour [c]. *)

val fg_gradient : Gradient.t -> t
(** [fg_gradient g] is the style whose foreground is [g], laid over the cells it
    styles: each cell takes the colour {!Gradient.cell} gives its row and
    column. *)

val bg_gradient : Gradient.t -> t
(** [bg_gradient g] is like {!fg_gradient} for the background. *)

val ( + ) : t -> t -> t
(** [s0 + s1] is [s0] with the attributes of [s1] added and the colours of [s1]
    replacing those of [s0]. *)

val foreground : t -> Color.t option
(** [foreground s] is the foreground colour of [s], if it sets one. A gradient
    foreground is its colour on row [0], column [0]. *)

val background : t -> Color.t option
(** [background s] is like {!foreground} for the background. *)

(** {1:preds Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none s] is [true] iff [s] sets nothing. *)

val has_gradient : t -> bool
(** [has_gradient s] is [true] iff the foreground or the background of [s] is a
    gradient, so that {!val-at} gives different styles on different cells. *)

val shows_on_space : t -> bool
(** [shows_on_space s] is [true] iff a space drawn in [s] looks different from
    an unstyled one: [s] sets a background, reverse video, underline or
    strikethrough. *)

val equal : t -> t -> bool
(** [equal s0 s1] is [true] iff [s0] and [s1] are the same style. *)

(** {1:cells Colours on cells} *)

val at : row:int -> column:int -> t -> t
(** [at ~row ~column s] is [s] on the cell at [row] and [column]: each gradient
    of [s] replaced by its colour on that cell, the rest of [s] unchanged. *)

val map_color : ?fg:(Color.t -> Color.t) -> ?bg:(Color.t -> Color.t) -> t -> t
(** [map_color ~fg ~bg s] is [s] with [fg] applied to its foreground colour and
    [bg] to its background colour (each defaults to the identity), every stop of
    a gradient included. *)

(** {1:ansi ANSI escapes} *)

val to_ansi : t -> string
(** [to_ansi s] is the SGR escape sequence that enables [at ~row:0 ~column:0 s],
    or [""] for {!none}. *)

val reset : string
(** [reset] is the SGR escape sequence that resets every attribute. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a style for diagnostics, in an unspecified format. *)
