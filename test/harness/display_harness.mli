(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Displays on a captured surface, for the {!Console.Display} tests. *)

open Console

val contains : string -> string -> bool
(** [contains affix s] is [true] iff [affix] occurs in [s]. *)

val clock : unit -> (unit -> float) * (float -> unit)
(** [clock ()] is a clock reading zero and a function that advances it. *)

val plain :
  ?header:string ->
  ?layout:Display.Line.t ->
  ?theme:Theme.t ->
  (Display.t -> (float -> unit) -> 'a) ->
  'a * string
(** [plain f] is the value of [f display advance] on a display that is not on a
    terminal, and what it wrote once closed. *)

type term = private {
  vte : Vte.t;
  display : Display.t;
  advance : float -> unit;
  size : (int * int) ref;
  chunks : string list ref;  (** Newest first. *)
}
(** The type for displays on an emulated terminal: every write the display makes
    is one chunk fed to [vte]. *)

val terminal :
  ?width:int ->
  ?height:int ->
  ?header:string ->
  ?layout:Display.Line.t ->
  ?theme:Theme.t ->
  ?styled:bool ->
  ?raw:bool ->
  ?before:string ->
  unit ->
  term
(** [terminal ()] is a display on a terminal of [width] by [height] (defaults to
    40 by 8), in colour iff [styled] (defaults to [false]). If [raw] (defaults
    to [false]) the terminal's line discipline is in raw mode: a line feed moves
    down and keeps the column. [before] (defaults to [""]) is what the host
    wrote on the terminal before the display started. *)

val written : term -> string
(** [written t] is every byte the display wrote, in order. *)

val resize : term -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the terminal and its emulator. *)

val screen : term -> string list
(** [screen t] are the rows of the screen, trailing blanks and blank rows at the
    bottom removed. *)

val lines : term -> string list
(** [lines t] are the scrollback and screen rows, trailing blanks and blank rows
    at the bottom removed. *)

val check_invalid : string -> (unit -> unit) -> unit
(** [check_invalid msg f] checks that [f ()] raises [Invalid_argument msg]. *)
