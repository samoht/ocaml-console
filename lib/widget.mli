(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

val border :
  ?theme:Theme.t -> ?border:Border.t -> default:Border.t -> unit -> Border.t
(** [border] resolves an explicit border, then a theme border, then [default].
*)

val one_line : Span.t -> Span.t
(** [one_line s] is [s] with each line feed a space. *)

val repeat : int -> string -> string
(** [repeat n g] is [n] copies of [g], or [""] if [n] is not positive. *)

val pad : int -> Span.t
(** [pad n] is [n] spaces, none if [n] is not positive. *)

val pp : (margin:int -> 'a -> Span.t list) -> 'a Fmt.t
(** [pp lines] formats the [lines] of a widget at the formatter's margin, one
    per line, with no line end after the last. *)

val to_string :
  (margin:int -> 'a -> Span.t list) -> ?styled:bool -> 'a -> string
(** [to_string lines v] is the [lines] of [v] at its natural width, joined by
    line feeds, styled iff [styled] (defaults to [false]). *)
