(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio runner for a {!Console.Anim.t}.

    {!run} drives a (possibly multi-line) animation on the terminal, redrawing
    it in place each tick while a function runs -- the live counterpart of
    sampling a {!Console.Anim.t} by hand. It is how {!Panel.rain} and friends
    actually move on screen. *)

val run :
  clock:_ Eio.Time.clock ->
  ?ppf:Format.formatter ->
  ?fps:float ->
  string Console.Anim.t ->
  (unit -> 'a) ->
  'a
(** [run ~clock a f] redraws [a]'s current frame in place [fps] times a second
    (default [10.]) while [f] runs, then clears it and returns [f]'s result. The
    frame may span several lines; the previous frame is erased and the new one
    rewritten each tick. The cursor is hidden for the duration and shown again
    when [f] returns, raises, or the process is interrupted with CTRL-C, so a
    SIGINT never leaves the terminal with a hidden cursor. If stdout is not a
    terminal the animation is suppressed and [f] runs directly, so piped output
    stays clean. If [f] raises, the frame is cleared and the exception
    re-raised. *)
