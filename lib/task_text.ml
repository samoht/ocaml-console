(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB" |]

(* [scaled n] is [(v, i)] such that [n] bytes are [v] of the unit
   [units.(i)], with [v] below 1024 unless [i] is the last unit. *)
let scaled n =
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, i)
    else pick (i + 1) (v /. 1024.)
  in
  pick 0 (Float.max 0. (Int64.to_float n))

let two n = if n < 10 then "0" ^ string_of_int n else string_of_int n

let bytes n =
  let v, i = scaled n in
  if i = 0 then string_of_int (int_of_float (Float.round v)) ^ " B"
  else
    let hundredths v = int_of_float (Float.round (v *. 100.)) in
    (* A count that rounds to 1024.00 of a unit is 1.00 of the next. *)
    let h, i =
      let h = hundredths v in
      if h < 102_400 || i = Array.length units - 1 then (h, i)
      else (hundredths (v /. 1024.), i + 1)
    in
    String.concat ""
      [ string_of_int (h / 100); "."; two (h mod 100); " "; units.(i) ]

let bytes_short n =
  let v, i = scaled n in
  let v, i =
    if v >= 999.5 && i < Array.length units - 1 then (v /. 1024., i + 1)
    else (v, i)
  in
  let unit = units.(i).[0] in
  if i = 0 then Fmt.str "%d%c" (int_of_float v) unit
  else if v < 9.95 then Fmt.str "%.1f%c" v unit
  else Fmt.str "%.0f%c" v unit

(* A table of every duration under a minute, since a display shows one per
   task at each frame. Each entry is made when first shown. The table itself
   is made at the first duration. If a domain reads an entry as "" while
   another domain writes it, the entry is made again. *)
let table = Atomic.make [||]

let tenth t =
  let a = Atomic.get table in
  let a =
    if Array.length a > 0 then a
    else
      let b = Array.make 600 "" in
      if Atomic.compare_and_set table [||] b then b else Atomic.get table
  in
  match a.(t) with
  | "" ->
      let s = string_of_int (t / 10) ^ "." ^ string_of_int (t mod 10) ^ "s" in
      a.(t) <- s;
      s
  | s -> s

let duration secs =
  let secs =
    if Float.is_nan secs then 0. else Float.min 1e15 (Float.max 0. secs)
  in
  let t = Float.round (secs *. 10.) in
  if t < 600. then tenth (int_of_float t)
  else
    let s = int_of_float (Float.round secs) in
    if s < 3600 then
      String.concat "" [ string_of_int (s / 60); "m"; two (s mod 60); "s" ]
    else
      let m = int_of_float (Float.round (secs /. 60.)) in
      String.concat "" [ string_of_int (m / 60); "h"; two (m mod 60); "m" ]

(* Below a hundred hours, [duration] is at most six cells wide ("59m59s",
   "99h59m"). A longer run takes the cells it needs instead of losing a
   digit. *)
let duration_cells = 6
let clean = Console_text.sanitize ~keep_newlines:false
let kept_bytes = 4096

(* ASCII dots, so a cut ASCII line stays ASCII. The sanitizer that runs at
   each redraw skips ASCII lines quickly. *)
let ellipsis = "..."

exception Cut of int

let capped s =
  if String.length s <= kept_bytes then s
  else
    let room = kept_bytes - String.length ellipsis in
    let cut =
      try
        Console_width.iter_clusters
          (fun ~offset ~len ~cells:_ ->
            if offset + len > room then raise_notrace (Cut offset))
          s;
        room
      with Cut i -> i
    in
    String.sub s 0 cut ^ ellipsis

let label s = capped (clean s)

let last_line_cells s =
  match String.rindex_opt s '\n' with
  | Some i ->
      Console_width.string_width
        (String.sub s (i + 1) (String.length s - i - 1))
  | None -> Console_width.string_width s

(* [expand_tabs s] is [s] sanitized, with each tab replaced by spaces up to
   the next stop of eight columns from the start of its line. A tab cannot be
   inside a character or a line end, so the pieces between tabs are sanitized
   separately. *)
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
             else col + stop + Console_width.string_width piece)
           (last_line_cells first) rest);
      Buffer.contents buf

let lines s =
  if s = "" then []
  else
    let lines = String.split_on_char '\n' (expand_tabs s) in
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
