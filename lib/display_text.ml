(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB" |]

let bytes n =
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, i)
    else pick (i + 1) (v /. 1024.)
  in
  let v, i = pick 0 (Float.max 0. (Int64.to_float n)) in
  if i = 0 then Fmt.str "%d B" (int_of_float (Float.round v))
  else
    (* Hundredths, rounded, carried into the whole part. *)
    let h = int_of_float (Float.round (v *. 100.)) in
    Fmt.str "%d.%02d %s" (h / 100) (h mod 100) units.(i)

let duration secs =
  let secs = if Float.is_nan secs then 0. else Float.max 0. secs in
  if secs < 60. then
    let tenths = int_of_float (secs *. 10.) in
    Fmt.str "%d.%ds" (tenths / 10) (tenths mod 10)
  else
    let s = int_of_float (Float.min secs 1e15) in
    if s < 3600 then Fmt.str "%dm%02ds" (s / 60) (s mod 60)
    else Fmt.str "%dh%02dm" (s / 3600) (s / 60 mod 60)

(* [duration] is six wide at its widest below a hundred hours ("59m59s",
   "23h59m"). A longer run takes the columns it needs rather than losing a
   digit. *)
let duration_cells = 6
let clean = Console_text.sanitize ~keep_newlines:false

(* The cells of the last line of [s]. *)
let last_line_cells s =
  match String.rindex_opt s '\n' with
  | Some i ->
      Width.string_width (String.sub s (i + 1) (String.length s - i - 1))
  | None -> Width.string_width s

(* [s] sanitized, a tab gone to the next stop of eight columns from the
   start of its line. A tab cannot be inside a character or a line end, so
   the text between two is sanitized alone. *)
let expand_tabs s =
  match String.split_on_char '\t' s with
  | [] -> ""
  | first :: rest ->
      let buf = Buffer.create (String.length s + 16) in
      let first = Console_text.sanitize first in
      Buffer.add_string buf first;
      ignore
        (List.fold_left
           (fun col piece ->
             let stop = 8 - (col mod 8)
             and piece = Console_text.sanitize piece in
             Buffer.add_string buf (String.make stop ' ');
             Buffer.add_string buf piece;
             if String.contains piece '\n' then last_line_cells piece
             else col + stop + Width.string_width piece)
           (last_line_cells first) rest);
      Buffer.contents buf

let lines s =
  if s = "" then []
  else
    let lines = String.split_on_char '\n' (expand_tabs s) in
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
