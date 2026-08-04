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

(** {1 Themes} *)

type bar = [ `Blocky | `Smooth | `Rainbow ]
(** A bar's look: full blocks on a shaded track, fractional eighth-cell fills,
    or a spectrum gradient. {!Bar} renders it. *)

type t
(** A theme: every visual choice the widgets share. *)

val accent : t -> Color.t
(** [accent t] is the default row, spinner, and bar colour. *)

val palette : t -> Color.t list
(** [palette t] is the colour cycle for successive rows and nodes. *)

val border : t -> Border.t
(** [border t] is the border for tables and panels. *)

val animated_border : t -> bool
(** [animated_border t] is [true] when panels and tables use rain animation. *)

val guide : t -> Guide.t
(** [guide t] is the guide for trees and nested display rows. *)

val spinner : t -> Spinner.t
(** [spinner t] is the running-row spinner. *)

val bar : t -> bar
(** [bar t] is the progress-bar style. *)

val done_marker : t -> string
(** [done_marker t] replaces the spinner on a completed row. *)

val ok_marker : t -> string
(** [ok_marker t] is the success verdict marker. *)

val fail_marker : t -> string
(** [fail_marker t] is the failure verdict marker. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] make the same visual choices in every
    field. *)

val pp : t Fmt.t
(** [pp] prints a short summary of a theme's markers, for diagnostics. *)

val v :
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?border:Border.t ->
  ?animated_border:bool ->
  ?guide:Guide.t ->
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
    bar, one restrained activity accent, and semantic success/failure markers.
*)

val matrix : t
(** Monochrome green-on-black: a single-line border and a braille spinner. *)

val rainbow : t
(** Rows and nodes cycle the spectrum, and the bar is a colour gradient. *)

val default : t
(** [default] is {!unicode}. *)
