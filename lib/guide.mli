(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Guide glyphs for nested terminal views. *)

type t
(** The four glyphs used to draw one level of nesting. *)

val v : branch:string -> last:string -> pipe:string -> space:string -> t
(** [v ~branch ~last ~pipe ~space] is a guide with the given glyphs. *)

val branch : t -> string
(** [branch t] is the non-final child connector. *)

val last : t -> string
(** [last t] is the final child connector. *)

val pipe : t -> string
(** [pipe t] is the continuation for an ancestor with following siblings. *)

val space : t -> string
(** [space t] is the continuation for a final ancestor. *)

val ascii : t
(** [ascii] uses portable ASCII glyphs. *)

val unicode : t
(** [unicode] uses Unicode box-drawing glyphs. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if every glyph is equal. *)

val pp : t Fmt.t
(** [pp] prints the four glyphs for diagnostics. *)
