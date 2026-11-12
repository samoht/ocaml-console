(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Guide glyphs for nested terminal views.

    A guide is the four glyphs that draw one level of a {!Tree} or of nested
    {!Progress} rows, and the style to draw them in. A control character in a
    glyph is drawn as a space. *)

(** {1:guides Guides} *)

type t
(** The type for the four glyphs that draw one level of nesting. *)

val v :
  ?style:Style.t ->
  branch:string ->
  last:string ->
  pipe:string ->
  space:string ->
  unit ->
  t
(** [v ~style ~branch ~last ~pipe ~space ()] is a guide with the given glyphs,
    each made safe by [Console_text.sanitize ~keep_newlines:false], drawn in
    [style] (defaults to {!Style.none}). Like a {!Border.v}, a guide in
    {!Style.none} is drawn in the muted colour of the formatter's theme
    ({!Theme.muted}). *)

val branch : t -> string
(** [branch t] is the connector before a child that is not the last. *)

val last : t -> string
(** [last t] is the connector before the last child. *)

val pipe : t -> string
(** [pipe t] is the glyph below an ancestor that has more siblings after it. *)

val space : t -> string
(** [space t] is the glyph below an ancestor that is the last child. *)

val style : t -> Style.t
(** [style t] is the style of [t]'s glyphs. {!Style.none} means the theme's
    muted colour. *)

val with_style : Style.t -> t -> t
(** [with_style s t] is [t] drawn in [s]. *)

(** {1:predefined Predefined guides} *)

val ascii : t
(** [ascii] is the guide drawn with [+--], [|] and spaces. *)

val single : t
(** [single] is the guide drawn with Unicode light box-drawing lines, as in
    {!Border.single}. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats the four glyphs for diagnostics. *)
