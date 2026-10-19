(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let bytes n =
  let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |] in
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, units.(i))
    else pick (i + 1) (v /. 1024.)
  in
  let v, u = pick 0 (Int64.to_float n) in
  if u = "B" then Fmt.str "%.0f %s" v u else Fmt.str "%.2f %s" v u

let span secs =
  if secs < 60. then Fmt.str "%.1fs" (Float.max 0. secs)
  else
    let s = int_of_float secs in
    if s < 3600 then Fmt.str "%dm%02ds" (s / 60) (s mod 60)
    else Fmt.str "%dh%02dm" (s / 3600) (s / 60 mod 60)

(* {!span} is six wide at its widest below a hundred hours ("59m59s",
   "23h59m"). A longer run takes the columns it needs rather than losing a
   digit. *)
let elapsed_cell = 6

(* Caller text is validated before it participates in width budgeting: a
   malformed sequence occupies a replacement-glyph cell in real terminals and
   controls could otherwise inject escapes or desynchronise redraw geometry. *)
let sanitize = Render.sanitize ~keep_newlines:false

let lines s =
  if s = "" then []
  else
    let lines = String.split_on_char '\n' s in
    let lines =
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
    in
    List.map
      (fun line ->
        let n = String.length line in
        if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line)
      lines

let sanitize_multiline s = String.concat "\n" (List.map sanitize (lines s))
