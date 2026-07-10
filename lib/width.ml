(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ANSI escape sequence detection *)
type ansi_state = Normal | Escape | Csi

(* Default cell-width hint: a control code takes no column and every other code
   point is assumed one cell. This is exact for ASCII and Latin text. A caller
   that renders wide text -- CJK, emoji -- injects a Unicode-aware hint through
   the rendering context. *)
let default_char_width u =
  let c = Uchar.to_int u in
  if c < 0x20 || (c >= 0x7f && c <= 0x9f) then 0 else 1

let update_width_state ~char_width ~width ~state ~zwj u =
  let c = Uchar.to_int u in
  match !state with
  | Normal ->
      if c = 0x1b then state := Escape
      else if c = 0x200d (* zero-width joiner *) then
        (* The next glyph fuses into this grapheme cluster -- an emoji ZWJ
           sequence like a family renders as a single cell, not the sum of its
           parts -- so it must add no width. *)
        zwj := true
      else if !zwj then zwj := false
      else width := !width + char_width u
  | Escape -> if c = 0x5b (* '[' *) then state := Csi else state := Normal
  | Csi ->
      (* CSI sequence ends with byte in range 0x40-0x7E *)
      if c >= 0x40 && c <= 0x7e then state := Normal

let string_width ?(char_width = default_char_width) str =
  let len = String.length str in
  let width = ref 0 in
  let state = ref Normal in
  let zwj = ref false in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        update_width_state ~char_width ~width ~state ~zwj u;
        loop ()
    | `End -> ()
    | `Await -> assert false
    | `Malformed _ -> loop ()
  in
  if len = 0 then 0
  else (
    loop ();
    !width)

let handle_truncate_normal ~char_width ~buf ~width ~target_width ~state u =
  let c = Uchar.to_int u in
  if c = 0x1b then begin
    state := Escape;
    Uutf.Buffer.add_utf_8 buf u;
    `Continue
  end
  else
    let w = char_width u in
    if !width + w <= target_width then begin
      width := !width + w;
      Uutf.Buffer.add_utf_8 buf u;
      `Continue
    end
    else `Stop

let handle_truncate_escape ~buf ~state u =
  let c = Uchar.to_int u in
  Uutf.Buffer.add_utf_8 buf u;
  if c = 0x5b then state := Csi else state := Normal

let handle_truncate_csi ~buf ~state u =
  let c = Uchar.to_int u in
  Uutf.Buffer.add_utf_8 buf u;
  if c >= 0x40 && c <= 0x7e then state := Normal

let handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u =
  match !state with
  | Normal ->
      handle_truncate_normal ~char_width ~buf ~width ~target_width ~state u
  | Escape ->
      handle_truncate_escape ~buf ~state u;
      `Continue
  | Csi ->
      handle_truncate_csi ~buf ~state u;
      `Continue

let rec truncate_loop ~char_width decoder ~buf ~width ~target_width ~state =
  match Uutf.decode decoder with
  | `Uchar u -> (
      match
        handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u
      with
      | `Continue ->
          truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
      | `Stop -> ())
  | `End -> ()
  | `Await -> assert false
  | `Malformed _ ->
      truncate_loop ~char_width decoder ~buf ~width ~target_width ~state

let truncate ?(char_width = default_char_width) target_width str =
  if target_width <= 0 then ""
  else
    let buf = Buffer.create (String.length str) in
    let width = ref 0 in
    let state = ref Normal in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    truncate_loop ~char_width decoder ~buf ~width ~target_width ~state;
    Buffer.contents buf

let pad_right ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else str ^ String.make (target_width - str_width) ' '

let pad_left ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else String.make (target_width - str_width) ' ' ^ str

let center ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else
    let total_pad = target_width - str_width in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    String.make left_pad ' ' ^ str ^ String.make right_pad ' '

let rec wrap_words ~effective acc line len = function
  | [] -> if line = "" then acc else line :: acc
  | word :: rest ->
      let wlen = String.length word in
      let space = if line = "" then 0 else 1 in
      if len + space + wlen <= effective then
        let line = if line = "" then word else line ^ " " ^ word in
        wrap_words ~effective acc line (len + space + wlen) rest
      else if line = "" then
        (* Word longer than width; accept it on its own line *)
        wrap_words ~effective (word :: acc) "" 0 rest
      else wrap_words ~effective (line :: acc) word wlen rest

let wrap ?(indent = 0) width text =
  let effective = width - indent in
  if effective <= 0 then text
  else
    let prefix = String.make indent ' ' in
    let normalized =
      text |> String.split_on_char '\n' |> List.map String.trim
      |> String.concat " "
    in
    let words = String.split_on_char ' ' normalized in
    let lines = List.rev (wrap_words ~effective [] "" 0 words) in
    String.concat "\n" (List.map (fun l -> prefix ^ l) lines)
