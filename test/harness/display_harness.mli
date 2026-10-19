(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sessions on a captured surface, for the {!Console.Display} tests. *)

open Console

val contains : string -> string -> bool
(** [contains affix s] is [true] iff [affix] occurs in [s]. *)

val fake_clock : unit -> (unit -> float) * (float -> unit)
(** [fake_clock ()] is a clock reading zero and a function that advances it. *)

val ctx :
  ?live:bool ->
  ?width:int ->
  ?height:int ->
  ppf:Format.formatter ->
  now:(unit -> float) ->
  unit ->
  Display.ctx
(** [ctx ~live ~width ~height ~ppf ~now ()] is a context on [ppf] (defaults to
    not a terminal, 80 by 24). *)

val capture :
  ?header:string ->
  ?layout:Display.Line.t ->
  ?theme:Theme.t ->
  (Display.t -> (float -> unit) -> 'a) ->
  'a * string
(** [capture f] is the value of [f] on an appending display over a fake clock,
    and the output it wrote. *)

val capture_live :
  ?header:string ->
  ?layout:Display.Line.t ->
  ?theme:Theme.t ->
  (Display.t -> (float -> unit) -> 'a) ->
  'a * string
(** [capture_live] is like {!capture} on a live display. *)

val check_invalid : string -> (unit -> unit) -> unit
(** [check_invalid msg f] checks that [f ()] raises [Invalid_argument msg]. *)

val mib : int -> int64
(** [mib n] is [n] MiB in bytes. *)

val gib : int -> int64
(** [gib n] is [n] GiB in bytes. *)

val terminal_run :
  ?height:int ->
  ?width:int ->
  ?header:string ->
  ?layout:Display.Line.t ->
  (Display.t -> (float -> unit) -> (unit -> unit) -> Vte.t -> unit) ->
  Vte.t
(** [terminal_run f] runs [f display advance sync vte] on a live display
    (defaults to 40 by 8) whose output feeds the emulator [vte], and is [vte]
    after the session closed. [sync ()] ticks the display and feeds the
    emulator. *)

val terminal_rows : Vte.t -> string list
(** [terminal_rows vte] are the scrollback and screen rows of [vte]. *)

val trailing : int -> string -> string
(** [trailing n row] are the last [n] bytes of [row]. *)

val screen_rows : Vte.t -> string list
(** [screen_rows vte] are the screen rows of [vte], trailing blanks removed. *)
