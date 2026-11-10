(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { vte : Vte.t; raw : bool }

let v ?(raw = false) ~rows ~cols () =
  { vte = Vte.create ~width_method:`Unicode_wide ~rows ~cols (); raw }

let vte t = t.vte
let rows t = Vte.rows t.vte
let cols t = Vte.cols t.vte
let resize t ~rows ~cols = ignore (Vte.resize t.vte ~rows ~cols)
let cursor_pos t = Vte.cursor_pos t.vte
let cursor_visible t = (Vte.modes t.vte).cursor_visible
let to_string t = Vte.to_string t.vte

let scrollback_lines t =
  let n = Vte.scrollback_size t.vte in
  List.init n (fun i -> Vte.text t.vte (i - n))

let scrollback_size t = Vte.scrollback_size t.vte
let grid t = Vte.grid t.vte
let fg = Matrix_grid.get_fg
let bg = Matrix_grid.get_bg

(* A tty with output processing on writes each line feed as CR LF. *)
let feed t s =
  let s =
    if t.raw then s else String.concat "\r\n" (String.split_on_char '\n' s)
  in
  ignore (Vte.feed_string t.vte s)

let cursor_history s =
  let t = v ~rows:24 ~cols:80 () in
  let hidden = ref false in
  String.iter
    (fun c ->
      feed t (String.make 1 c);
      if not (cursor_visible t) then hidden := true)
    s;
  (!hidden, cursor_visible t)

let synchronized_open s =
  let mode = "\027[?2026" in
  let m = String.length mode in
  let rec go i last =
    if i + m >= String.length s then last
    else if String.sub s i m = mode then
      go (i + m) (match s.[i + m] with 'h' -> true | 'l' -> false | _ -> last)
    else go (i + 1) last
  in
  go 0 false

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let drop_trailing_blanks rows =
  let rec go = function "" :: tl -> go tl | rows -> rows in
  List.rev (go (List.rev rows))

let screen ?(scrollback = false) t =
  let visible = String.split_on_char '\n' (Vte.to_string t.vte) in
  let rows = if scrollback then scrollback_lines t @ visible else visible in
  drop_trailing_blanks (List.map rstrip rows)

(* One record's header is 24 bytes: the payload length, the wall clock the
   capture stamped it with, and which of the three kinds it is. *)
let header_size = 24
let u64 s off = Int64.to_int (String.get_int64_le s off)
let i32 s off = Int32.to_int (String.get_int32_le s off)

let records recording =
  let n = String.length recording in
  let rec loop off acc =
    (* A capture of a program that was still running ends mid-record, so a
       header that does not fit, or one promising more payload than is left,
       ends the stream rather than raising. *)
    if off + header_size > n then List.rev acc
    else
      let len = u64 recording off in
      if len < 0 || len > n - off - header_size then List.rev acc
      else
        let seconds =
          float_of_int (u64 recording (off + 8))
          +. (float_of_int (i32 recording (off + 16)) /. 1e6)
        in
        let kind = Char.chr (i32 recording (off + 20) land 0xff) in
        let next = off + header_size + len in
        if kind <> 'o' || len = 0 then loop next acc
        else
          loop next
            ((seconds, String.sub recording (off + header_size) len) :: acc)
  in
  loop 0 []

let frames ~rows ~cols recording =
  match records recording with
  | [] -> []
  | (start, _) :: _ as records ->
      let t = v ~rows ~cols () in
      List.map
        (fun (seconds, written) ->
          feed t written;
          (seconds -. start, screen t))
        records
