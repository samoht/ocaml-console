(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let drop_trailing_blanks rows =
  let rec go = function "" :: tl -> go tl | rows -> rows in
  List.rev (go (List.rev rows))

let screen ?(scrollback = false) vte =
  let visible = String.split_on_char '\n' (Vte.to_string vte) in
  let rows =
    if scrollback then Vte.scrollback_lines vte @ visible else visible
  in
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
      if len < 0 || off + header_size + len > n then List.rev acc
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
      let vte = Vte.create ~rows ~cols () in
      List.map
        (fun (seconds, written) ->
          Vte.feed_string vte written;
          (seconds -. start, screen vte))
        records
