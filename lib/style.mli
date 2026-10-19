(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    A style is a set of attributes and colours, composed with {!( + )}:
    [Style.(bold + fg Color.red + underline)]. *)

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

val ( + ) : t -> t -> t
(** [s0 + s1] is [s0] with the attributes of [s1] added and the colours of [s1]
    replacing those of [s0]. *)

val foreground : t -> Color.t option
(** [foreground s] is the foreground colour of [s], if it sets one. *)

val background : t -> Color.t option
(** [background s] is like {!foreground} for the background. *)

(** {1:preds Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none s] is [true] iff [s] sets nothing. *)

val equal : t -> t -> bool
(** [equal s0 s1] is [true] iff [s0] and [s1] are the same style. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a style for diagnostics, in an unspecified format. *)

(**/**)

val shows_on_space : t -> bool
(** [shows_on_space s] is [true] iff a space drawn in [s] looks different from
    an unstyled one. *)

val to_ansi : ?depth:Color.depth -> t -> string
(** [to_ansi ~depth s] is the escape code that starts [s], its colours as a
    terminal of [depth] shows them (defaults to [`True_color]), or [""] for
    {!none}. *)

val reset : string
(** [reset] is the escape code that turns every attribute off. *)

(**/**)
