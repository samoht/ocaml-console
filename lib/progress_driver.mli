(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Showing tasks for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console.driver] reads it through [Console__]. *)

val show :
  write_error:(exn -> bool) ->
  ?cancelled:(exn -> bool) ->
  ?atomic:((unit -> unit) -> unit) ->
  ?progress:Progress.t ->
  ?layout:Progress.Layout.t ->
  ?view:View.t ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task.t ->
  Display.t
(** [show ~write_error ~cancelled ~atomic ppf root] is
    {!Progress.show}[ ppf root], whose display ends its output on an exception
    for which [write_error] is [true], such as a closed pipe, does not keep for
    later an exception for which [cancelled] is [true], and writes each update
    through [atomic], which may keep a cancellation from cutting it short. *)
