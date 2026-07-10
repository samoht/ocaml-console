(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio runner for a {!Console.Spinner}.

    {!run} animates a spinner on one line while a function runs -- the simple
    case of a single indeterminate task, where a full {!Console.Display} tree
    would be overkill. *)

val run :
  clock:_ Eio.Time.clock ->
  ?ppf:Format.formatter ->
  ?spinner:Console.Spinner.t ->
  ?fps:float ->
  ?label:string ->
  (unit -> 'a) ->
  'a
(** [run ~clock f] animates a spinner on a single line while [f] runs, then
    clears the line and returns [f]'s result. The frame advances at [fps] frames
    per second (default [10.]); [spinner] defaults to {!Console.Spinner.braille}
    and the line reads [<frame> <label>]. If stdout is not a terminal the
    spinner is suppressed and [f] runs directly, so piped output stays clean. If
    [f] raises, the line is cleared and the exception re-raised. *)
