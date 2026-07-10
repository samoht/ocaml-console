(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared visual theme.

    A theme bundles every visual choice the widgets share -- the accent colour
    and palette, the box-drawing border, the tree guide, and the live display's
    spinner, bar style and markers -- so one value styles tables, trees, panels
    and the progress display consistently. It is pure data: the widgets render
    {e with} it ({!Bar.render}, {!Tree}, {!Table}, {!Panel}, {!Display}); it
    renders nothing itself. *)

type color =
  [ `Cyan
  | `Green
  | `Blue
  | `Magenta
  | `Yellow
  | `Red
  | `White
  | `Hex of int * int * int ]
(** A foreground colour, named or 24-bit [`Hex (r, g, b)]. *)

(** {1 Tree guides} *)

type guide = { branch : string; last : string; pipe : string; space : string }
(** The four glyphs that draw a tree's nesting: a branch before a non-final
    child, a final-child marker, a vertical pipe for an ancestor with more
    siblings below, and blank space for an ancestor with none. *)

val ascii_guide : guide
(** [ascii_guide] is the ASCII tree guide. *)

val unicode_guide : guide
(** [unicode_guide] is the Unicode tree guide. *)

(** {1 Themes} *)

type bar = [ `Blocky | `Smooth | `Rainbow ]
(** A bar's look: full blocks on a shaded track, fractional eighth-cell fills,
    or a spectrum gradient. {!Bar} renders it. *)

type t = {
  accent : color;  (** The default row/spinner/bar accent. *)
  palette : color list;
      (** Cycled to give successive rows/nodes distinct accents; [[]] means
          none. *)
  border : Border.t;  (** Border for tables and panels. *)
  animated_border : bool;
      (** When [true], {!Panel.anim} and {!Table.anim} render the border as
          Matrix-style digital rain instead of a still frame. *)
  guide : guide;  (** Guide for trees and the live display's nesting. *)
  spinner : Spinner.t;  (** The spinner animated beside a running row. *)
  bar : bar;  (** The bar style rows fill with. *)
  done_marker : string;  (** Replaces the spinner on a finished row. *)
  ok_marker : string;  (** Result-banner success verdict, e.g. ["[OK]"]. *)
  fail_marker : string;  (** Result-banner failure verdict, e.g. ["[FAIL]"]. *)
}
(** A theme: every visual choice the widgets share, as data. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] make the same visual choices in every
    field. *)

val pp : t Fmt.t
(** [pp] prints a short summary of a theme's markers, for diagnostics. *)

val v :
  ?accent:color ->
  ?palette:color list ->
  ?border:Border.t ->
  ?animated_border:bool ->
  ?guide:guide ->
  ?bar:bar ->
  spinner:Spinner.t ->
  done_marker:string ->
  unit ->
  t
(** [v ~spinner ~done_marker ()] is a custom theme; the other fields take modern
    defaults. *)

val with_verdict : ?ok:string -> ?fail:string -> t -> t
(** [with_verdict ?ok ?fail t] is [t] with the result-banner verdict markers
    replaced. *)

val dos : t
(** [dos] is amber DOS satellite-workstation CRT: ASCII everything, a blocky
    full-block bar, a [+] done marker, and an [[OK]] / [[FAIL]] result banner.
*)

val unicode : t
(** Modern unicode: a rounded border, a braille spinner, a smooth fractional
    bar, a check-mark done marker, and a green tick / red cross result banner.
*)

val matrix : t
(** Monochrome green-on-black: a single-line border and a braille spinner. *)

val rainbow : t
(** Rows and nodes cycle the spectrum, and the bar is a colour gradient. *)

val default : t
(** [default] is {!dos}. *)
