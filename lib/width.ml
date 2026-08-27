(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ANSI escape sequence detection *)
type ansi_state = Normal | Escape | Csi | Osc | Osc_escape

let next_ansi_state state c =
  match state with
  | Normal -> if c = 0x1b then Escape else Normal
  | Escape ->
      if c = 0x5b (* '[' *) then Csi
      else if c = 0x5d (* ']' *) then Osc
      else Normal
  | Csi ->
      (* CSI sequence ends with a byte in the range 0x40-0x7E. *)
      if c >= 0x40 && c <= 0x7e then Normal else Csi
  | Osc ->
      if c = 0x07 (* BEL *) then Normal
      else if c = 0x1b then Osc_escape
      else Osc
  | Osc_escape -> if c = 0x5c (* '\\' *) then Normal else Osc

(* The per-codepoint fallback used for an explicitly supplied width function.
   Default string measurement below is grapheme-aware: terminals render a
   cluster, not the sum of its scalar values. *)
let default_char_width u =
  match Uucp.Break.tty_width_hint u with -1 -> 0 | width -> width

let update_width_state ~char_width ~width ~state ~zwj u =
  let c = Uchar.to_int u in
  match !state with
  | Normal ->
      if c = 0x1b then state := next_ansi_state Normal c
      else if c = 0x200d (* zero-width joiner *) then
        (* The next glyph fuses into this grapheme cluster -- an emoji ZWJ
           sequence like a family renders as a single cell, not the sum of its
           parts -- so it must add no width. *)
        zwj := true
      else if !zwj then zwj := false
      else width := !width + char_width u
  | current -> state := next_ansi_state current c

let codepoint_string_width ~char_width str =
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
    | `Malformed _ ->
        update_width_state ~char_width ~width ~state ~zwj Uutf.u_rep;
        loop ()
  in
  if len = 0 then 0
  else (
    loop ();
    !width)

let ansi_end str start =
  let len = String.length str in
  if start + 1 >= len then len
  else
    match str.[start + 1] with
    | '[' ->
        let rec csi i =
          if i >= len then len
          else
            let c = Char.code str.[i] in
            if c >= 0x40 && c <= 0x7e then i + 1 else csi (i + 1)
        in
        csi (start + 2)
    | ']' ->
        let rec osc i =
          if i >= len then len
          else if str.[i] = '\007' then i + 1
          else if i + 1 < len && str.[i] = '\027' && str.[i + 1] = '\\' then
            i + 2
          else osc (i + 1)
        in
        osc (start + 2)
    | next when Char.code next >= 0x80 ->
        let decoded = String.get_utf_8_uchar str (start + 1) in
        min len (start + 1 + Uchar.utf_decode_length decoded)
    | _ -> min len (start + 2)

let grapheme_string_width str =
  (* Styling controls do not break a terminal grapheme. Strip them before
     segmentation so a combining mark or ZWJ sequence spanning an ANSI reset
     has exactly the same width here as it does in the terminal. Measuring each
     visible run separately over-counts standalone combining marks. *)
  let visible = Buffer.create (String.length str) in
  let rec copy i =
    if i >= String.length str then ()
    else if str.[i] = '\027' then copy (ansi_end str i)
    else begin
      Buffer.add_char visible str.[i];
      copy (i + 1)
    end
  in
  copy 0;
  Glyph.String.measure ~width_method:`Unicode ~tab_width:2
    (Buffer.contents visible)

let string_width ?(char_width = default_char_width) str =
  if char_width == default_char_width then grapheme_string_width str
  else codepoint_string_width ~char_width str

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

let handle_truncate_ansi ~buf ~state u =
  let c = Uchar.to_int u in
  Uutf.Buffer.add_utf_8 buf u;
  state := next_ansi_state !state c;
  `Continue

let handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u =
  match !state with
  | Normal ->
      handle_truncate_normal ~char_width ~buf ~width ~target_width ~state u
  | Escape | Csi | Osc | Osc_escape -> handle_truncate_ansi ~buf ~state u

let rec truncate_loop ~char_width decoder ~buf ~width ~target_width ~state =
  match Uutf.decode decoder with
  | `Uchar u -> (
      match
        handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u
      with
      | `Continue ->
          truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
      | `Stop -> true)
  | `End -> false
  | `Await -> assert false
  | `Malformed _ -> (
      match
        handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state
          Uutf.u_rep
      with
      | `Continue ->
          truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
      | `Stop -> true)

let starts_at str prefix at =
  let plen = String.length prefix in
  at + plen <= String.length str && String.sub str at plen = prefix

let contains str prefix =
  let rec loop at =
    at + String.length prefix <= String.length str
    && (starts_at str prefix at || loop (at + 1))
  in
  loop 0

let hyperlink_is_open str =
  let prefix = "\027]8;;" and terminator = "\027\\" in
  let rec find_terminator at =
    if at + 1 >= String.length str then None
    else if starts_at str terminator at then Some at
    else find_terminator (at + 1)
  in
  let rec loop at open_ =
    if at + String.length prefix > String.length str then open_
    else if starts_at str prefix at then
      let uri_start = at + String.length prefix in
      match find_terminator uri_start with
      | None -> true
      | Some stop -> loop (stop + String.length terminator) (stop > uri_start)
    else loop (at + 1) open_
  in
  loop 0 false

let normalize_utf8 str =
  let buffer = Buffer.create (String.length str) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        Uutf.Buffer.add_utf_8 buffer u;
        loop ()
    | `Malformed _ ->
        Uutf.Buffer.add_utf_8 buffer Uutf.u_rep;
        loop ()
    | `End -> Buffer.contents buffer
    | `Await -> assert false
  in
  loop ()

let truncate_graphemes target_width str =
  let str = normalize_utf8 str in
  let buffer = Buffer.create (String.length str) in
  let width = ref 0 and stopped = ref false in
  let append_visible run =
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        if not !stopped then
          let grapheme_width =
            Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:2 run
              ~pos:offset ~len
          in
          if !width + grapheme_width <= target_width then (
            Buffer.add_substring buffer run offset len;
            width := !width + grapheme_width)
          else stopped := true)
      run
  in
  let visible_run_end str start =
    let rec loop i =
      if i >= String.length str || str.[i] = '\027' then i else loop (i + 1)
    in
    loop start
  in
  let rec loop i =
    if !stopped || i >= String.length str then ()
    else if str.[i] = '\027' then (
      let stop = ansi_end str i in
      Buffer.add_substring buffer str i (stop - i);
      loop stop)
    else
      let stop = visible_run_end str i in
      append_visible (String.sub str i (stop - i));
      loop stop
  in
  loop 0;
  if !stopped then begin
    let rendered = Buffer.contents buffer in
    if contains rendered "\027[" then Buffer.add_string buffer "\027[0m";
    if hyperlink_is_open rendered then Buffer.add_string buffer "\027]8;;\027\\"
  end;
  Buffer.contents buffer

let truncate_codepoints ~char_width target_width str =
  if target_width <= 0 then ""
  else
    let buf = Buffer.create (String.length str) in
    let width = ref 0 in
    let state = ref Normal in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let truncated =
      truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
    in
    if truncated then begin
      let rendered = Buffer.contents buf in
      if contains rendered "\027[" then Buffer.add_string buf "\027[0m";
      if hyperlink_is_open rendered then Buffer.add_string buf "\027]8;;\027\\"
    end;
    Buffer.contents buf

let truncate ?(char_width = default_char_width) target_width str =
  if target_width <= 0 then ""
  else if char_width == default_char_width then
    truncate_graphemes target_width str
  else truncate_codepoints ~char_width target_width str

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

let rec wrap_words ~char_width ~effective acc line len = function
  | [] -> if line = "" then acc else line :: acc
  | word :: rest ->
      let wlen = string_width ~char_width word in
      let space = if line = "" then 0 else 1 in
      if len + space + wlen <= effective then
        let line = if line = "" then word else line ^ " " ^ word in
        wrap_words ~char_width ~effective acc line (len + space + wlen) rest
      else if line = "" then
        (* Word longer than width; accept it on its own line *)
        wrap_words ~char_width ~effective (word :: acc) "" 0 rest
      else wrap_words ~char_width ~effective (line :: acc) word wlen rest

let wrap ?(char_width = default_char_width) ?(indent = 0) width text =
  let effective = width - indent in
  if effective <= 0 then text
  else
    let prefix = String.make indent ' ' in
    let normalized =
      text |> String.split_on_char '\n' |> List.map String.trim
      |> String.concat " "
    in
    let words = String.split_on_char ' ' normalized in
    let lines = List.rev (wrap_words ~char_width ~effective [] "" 0 words) in
    String.concat "\n" (List.map (fun l -> prefix ^ l) lines)
