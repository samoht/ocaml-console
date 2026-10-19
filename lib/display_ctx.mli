(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Display contexts.

    A context is what a display needs from its environment: where to write, a
    clock and how to wait on it, the terminal geometry and whether the output is
    a terminal. This is the representation behind {!Display.ctx}. *)

type t = {
  ppf : Format.formatter;  (** The output. *)
  now : unit -> float;  (** The clock, in seconds. *)
  wait : (float -> unit) option;  (** Waits on [now], if it can be. *)
  dimensions : unit -> int * int;  (** The terminal width and height. *)
  is_tty : bool;  (** Whether a live region can be drawn. *)
  char_width : (Uchar.t -> int) option;  (** Cell measurement override. *)
}
(** The type for contexts. *)

val v :
  ppf:Format.formatter ->
  now:(unit -> float) ->
  ?wait:(float -> unit) ->
  dimensions:(unit -> int * int) ->
  is_tty:bool ->
  unit ->
  t
(** [v ~ppf ~now ~wait ~dimensions ~is_tty ()] is a context with those fields
    and no {!field-char_width}. *)

val ppf : t -> Format.formatter
(** [ppf c] is [c.ppf]. *)

val now : t -> float
(** [now c] is [c.now ()]. *)

val with_char_width : t -> (Uchar.t -> int) -> t
(** [with_char_width c w] is [c] measuring cells with [w]. *)

val pp : t Fmt.t
(** [pp] formats a context for diagnostics, in an unspecified format. *)

val dumb : unit -> t
(** [dumb ()] is an 80 by 24 context on {!Format.std_formatter} that is not a
    terminal and whose clock reads zero. *)
