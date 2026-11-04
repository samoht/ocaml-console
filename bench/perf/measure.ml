(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The words allocated by the calling domain so far: minor plus major, less
   what was promoted, which the minor count has already seen. *)
let words () =
  let minor, promoted, major = Gc.counters () in
  minor +. major -. promoted

let wall = Unix.gettimeofday

(* A formatter that discards what it is given, counting the frames the
   display wrote: a frame is one synchronized update. *)
type sink = { ppf : Format.formatter; frames : int ref }

let sync_on = "\027[?2026h"

let starts_sync s o l =
  let n = String.length sync_on in
  l >= n
  &&
  let rec go i = i = n || (s.[o + i] = sync_on.[i] && go (i + 1)) in
  go 0

let sink () =
  let frames = ref 0 in
  let out s o l = if starts_sync s o l then incr frames in
  let ppf = Format.make_formatter out ignore in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Fmt.set_utf_8 ppf true;
  { ppf; frames }

(* [per_call ~n f] runs [f i] for [i] in [1..n] and is the processor
   nanoseconds and the words allocated per call. *)
let per_call ~n f =
  for i = 1 to min n 1000 do
    f i
  done;
  let w0 = words () and t0 = Sys.time () in
  for i = 1 to n do
    f i
  done;
  let t1 = Sys.time () and w1 = words () in
  ((t1 -. t0) *. 1e9 /. float n, (w1 -. w0) /. float n)

let failures = ref 0

(* [verdict ~gated ~ratchet ?target w] says how [w] words compare with
   their budget. If [gated] (the default), a count over the ratchet once
   rounded, as it is printed, is a failure; otherwise the ratchet is only
   the count recorded, printed beside it. *)
let verdict ?(gated = true) ~ratchet ?target w =
  let target =
    match target with Some t -> Fmt.str ", target %.0f" t | None -> ""
  in
  if Float.round w <= ratchet then
    Fmt.str "%s %.0f%s" (if gated then "ratchet" else "recorded") ratchet target
  else if gated then begin
    incr failures;
    Fmt.str "OVER ratchet %.0f%s" ratchet target
  end
  else Fmt.str "over recorded %.0f%s, not gated" ratchet target

let exit_on_failures () =
  if !failures > 0 then begin
    Fmt.epr "%d case(s) over budget@." !failures;
    exit 1
  end
