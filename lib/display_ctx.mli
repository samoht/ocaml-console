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
  wait : float -> unit;  (** Waits on {!field-now}. *)
  dimensions : unit -> int * int;  (** The terminal width and height. *)
  live : bool;  (** Whether a live region can be drawn. *)
  depth : Color.depth;  (** The colours the terminal shows. *)
}
(** The type for contexts. *)

val v :
  ?depth:Color.depth ->
  ppf:Format.formatter ->
  now:(unit -> float) ->
  wait:(float -> unit) ->
  dimensions:(unit -> int * int) ->
  live:bool ->
  unit ->
  t
(** [v ~depth ~ppf ~now ~wait ~dimensions ~live ()] is a context with those
    fields. *)

val pp : t Fmt.t
(** [pp] formats a context for diagnostics, in an unspecified format. *)
