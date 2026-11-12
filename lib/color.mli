(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal colours.

    A colour is one of the 16 named ANSI colours, an entry of the xterm
    256-colour palette, or a 24-bit RGB colour.

    A colour is stored as given. When printed, it is converted to the
    {!val-depth} of the formatter ({!downsample}). *)

(** {1:colours Colours} *)

type t
(** The type for terminal colours. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is the 24-bit colour with red [r], green [g] and blue [b].

    @raise Invalid_argument if a component is not in \[[0];[255]\]. *)

val of_hex : string -> (t, [ `Msg of string ]) result
(** [of_hex s] is the colour written [s] in hexadecimal: ["RRGGBB"] or ["RGB"],
    optionally after a ["#"]. ["RGB"] is ["RRGGBB"] with each digit doubled. *)

val ansi_256 : int -> t
(** [ansi_256 n] is the entry [n] of the xterm 256-colour palette.

    @raise Invalid_argument if [n] is not in \[[0];[255]\]. *)

(** {1:predefined Predefined colours} *)

val black : t
(** [black] is the terminal's black, ANSI colour 0. *)

val red : t
(** [red] is the terminal's red, ANSI colour 1. *)

val green : t
(** [green] is the terminal's green, ANSI colour 2. *)

val yellow : t
(** [yellow] is the terminal's yellow, ANSI colour 3. *)

val blue : t
(** [blue] is the terminal's blue, ANSI colour 4. *)

val magenta : t
(** [magenta] is the terminal's magenta, ANSI colour 5. *)

val cyan : t
(** [cyan] is the terminal's cyan, ANSI colour 6. *)

val white : t
(** [white] is the terminal's white, ANSI colour 7. *)

val bright_black : t
(** [bright_black] is the terminal's bright black, ANSI colour 8. *)

val bright_red : t
(** [bright_red] is the terminal's bright red, ANSI colour 9. *)

val bright_green : t
(** [bright_green] is the terminal's bright green, ANSI colour 10. *)

val bright_yellow : t
(** [bright_yellow] is the terminal's bright yellow, ANSI colour 11. *)

val bright_blue : t
(** [bright_blue] is the terminal's bright blue, ANSI colour 12. *)

val bright_magenta : t
(** [bright_magenta] is the terminal's bright magenta, ANSI colour 13. *)

val bright_cyan : t
(** [bright_cyan] is the terminal's bright cyan, ANSI colour 14. *)

val bright_white : t
(** [bright_white] is the terminal's bright white, ANSI colour 15. *)

(** {1:rgb RGB} *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the red, green and blue components of [c]. A named or palette
    colour gives xterm's default value. A terminal's own colour scheme may use
    other values. *)

val blend : t -> t -> float -> t
(** [blend c0 c1 f] is the RGB colour a fraction [f] of the way from [c0] to
    [c1]. [f] is clamped to the range 0 to 1, and NaN counts as 0. Each
    component is [a + round (f * (b - a))]. So [blend c0 c1 0.] is [c0] and
    [blend c0 c1 1.] is [c1], as RGB. The fraction comes last, so [blend c0 c1]
    is the gradient from [c0] to [c1]. {!scale} takes its factor first. *)

val gradient : t list -> float -> t
(** [gradient stops f] is the colour a fraction [f] of the way along [stops].
    The stops are spread evenly from [0.] to [1.]. The result is the {!blend} of
    the two stops around [f]. When [f] falls exactly on a stop, the result is
    that stop as given, so a named colour stays named. [f] is clamped to the
    range 0 to 1, and NaN counts as 0. So [gradient [c0; c1]] is [blend c0 c1],
    except at its ends. {!View.gradient} colours a line with [gradient stops],
    from its first cluster to its last.

    @raise Invalid_argument if [stops] is empty. *)

val scale : float -> t -> t
(** [scale f c] is the RGB colour with the components of [c] multiplied by [f].
    Each component is rounded and clamped to 0 to 255, and NaN gives 0. The
    factor comes first, so [scale 0.5] is a function that darkens by half.
    {!blend} takes its fraction last. *)

(** {1:preds Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal c0 c1] is [true] iff [c0] and [c1] are the same colour as given. For
    example, {!val-red} and [ansi_256 1] are not equal, although a terminal may
    draw them the same. *)

val compare : t -> t -> int
(** [compare c0 c1] is a total order on colours, compatible with {!equal}. *)

(** {1:depths Colour depth} *)

type depth = [ `Ansi_16 | `Ansi_256 | `True_color ]
(** The type for colour depths: the 16 named colours, the xterm 256-colour
    palette, or any 24-bit colour. *)

val depth : Format.formatter -> depth
(** [depth ppf] is the colour depth of [ppf]. Every printer of this library
    {!downsample}s the colours it writes on [ppf] to this depth. It is
    [`Ansi_16], which every terminal shows, until {!set_depth} is called on
    [ppf]. [Console_tty.setup] sets it from the environment. *)

val set_depth : Format.formatter -> depth -> unit
(** [set_depth ppf d] sets the colour depth of [ppf] to [d]. *)

val downsample : depth -> t -> t
(** [downsample d c] is [c] as a terminal of depth [d] shows it:
    - at [`True_color], or when [c] is already in the palette of [d], it is [c];
    - at [`Ansi_256], it is the nearest entry of the colour cube or of the grey
      ramp, whichever is nearer in RGB, as in tmux's [colour_find_rgb];
    - at [`Ansi_16], it is the named colour nearest by the CIE 1976 colour
      difference (the distance in CIE L*a*b*, D65 white), using the xterm
      defaults of the 16 named colours.

    It is idempotent: [downsample d (downsample d c)] is [downsample d c]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats an RGB colour as [#rrggbb], which {!of_hex} reads back. It
    formats a named or palette colour by its name or number. *)

(**/**)

val sgr : depth -> t -> Sgr.color
(** [sgr d c] is [downsample d c], as an [Sgr.color]. *)
