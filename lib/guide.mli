(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Guide glyphs for nested terminal views.

    A guide is the four glyphs that draw one level of a {!Tree} or of nested
    {!Display} rows, and the style they are drawn in. A control character in a
    glyph is drawn as a space. *)

(** {1:guides Guides} *)

type t
(** The type for the four glyphs that draw one level of nesting. *)

val v : branch:string -> last:string -> pipe:string -> space:string -> t
(** [v ~branch ~last ~pipe ~space] is a guide with the given glyphs, drawn in
    the muted colour of the theme of the formatter it is printed on
    ({!Theme.muted}) until {!with_style} gives it a style other than
    {!Style.none}. *)

val branch : t -> string
(** [branch t] is the non-final child connector. *)

val last : t -> string
(** [last t] is the final child connector. *)

val pipe : t -> string
(** [pipe t] is the continuation for an ancestor with following siblings. *)

val space : t -> string
(** [space t] is the continuation for a final ancestor. *)

val style : t -> Style.t
(** [style t] is the style [t]'s glyphs are drawn in, {!Style.none} for the
    theme's muted colour. *)

val with_style : Style.t -> t -> t
(** [with_style s t] is [t] drawn in [s]. *)

(** {1:predefined Predefined guides} *)

val ascii : t
(** [ascii] is the guide drawn with [+--], [|] and spaces. *)

val unicode : t
(** [unicode] is the guide drawn with Unicode light box-drawing lines. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats the four glyphs for diagnostics. *)
