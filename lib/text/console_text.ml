(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The escape scanner follows the DEC parser of Paul Williams's state diagram
   (vt100.net/emu/dec_ansi_parser) over ECMA-48 and ECMA-35. *)

let removed u =
  let c = Uchar.to_int u in
  (c >= 0xfff9 && c <= 0xfffb)
  || Uucp.Gen.is_default_ignorable u
     && c <> 0x200c && c <> 0x200d
     && not (Uucp.Gen.is_variation_selector u)

let is_selector u = Uucp.Gen.is_variation_selector u

(* The state that [sanitize] carries from one character to the next. [cr] is
   set after a CR, so that a line feed right after it adds no second line end.
   [base] is set where a variation selector may follow. This includes the start
   of the text, because the base may be at the end of the text before it. A
   removed character changes neither field. *)
type seen = { mutable cr : bool; mutable base : bool }

(* [is_base u c] is [true] iff a variation selector may follow [u], whose code
   point is [c]. *)
let is_base u c =
  c > 0x20
  && (not (c >= 0x7f && c <= 0x9f))
  && (c < 0x80 || not (is_selector u))
  && c <> 0x200c && c <> 0x200d

(* [text buf seen u] adds to [buf] what [sanitize] makes of the character [u]:
   nothing if [u] is {!removed} or a variation selector with no base; a line
   end for CR or a line feed (LF, VT, FF and NEL, U+0085); a space for any
   other control; otherwise [u] itself. *)
let text buf ~keep_newlines seen u =
  let c = Uchar.to_int u in
  let line_end () = Buffer.add_char buf (if keep_newlines then '\n' else ' ') in
  let after_cr = seen.cr in
  let ascii = c < 0x80 in
  let dropped =
    (not ascii) && (removed u || (is_selector u && not seen.base))
  in
  if not dropped then begin
    seen.cr <- c = 0x0d;
    seen.base <- is_base u c
  end;
  if c = 0x0d then line_end ()
  else if (c >= 0x0a && c <= 0x0c) || c = 0x85 then (
    if not after_cr then line_end ())
  else if dropped then ()
  else if c >= 0x20 && not (c >= 0x7f && c <= 0x9f) then
    Buffer.add_utf_8_uchar buf u
  else Buffer.add_char buf ' '

let subdivision_flags =
  let flag code =
    let b = Buffer.create 28 in
    Buffer.add_utf_8_uchar b (Uchar.of_int 0x1f3f4);
    String.iter
      (fun ch ->
        Buffer.add_utf_8_uchar b (Uchar.of_int (0xe0000 + Char.code ch)))
      code;
    Buffer.add_utf_8_uchar b (Uchar.of_int 0xe007f);
    Buffer.contents b
  in
  List.map flag [ "gbeng"; "gbsct"; "gbwls" ]

let flag_length = String.length (List.hd subdivision_flags)

(* [tag_sequence str i d] is [Some stop] if one of {!subdivision_flags}
   starts at [i] and ends at [stop]. [d] is the character decoded at [i]. *)
let tag_sequence str i d =
  if
    Uchar.to_int (Uchar.utf_decode_uchar d) = 0x1f3f4
    && i + flag_length <= String.length str
    && List.mem (String.sub str i flag_length) subdivision_flags
  then Some (i + flag_length)
  else None

let sanitize_into buf ~keep_newlines seen str =
  let n = String.length str in
  let rec go i =
    if i < n then begin
      let d = String.get_utf_8_uchar str i in
      if not (Uchar.utf_decode_is_valid d) then begin
        seen.cr <- false;
        seen.base <- true;
        Buffer.add_utf_8_uchar buf Uchar.rep;
        go (i + Uchar.utf_decode_length d)
      end
      else
        match tag_sequence str i d with
        | Some stop ->
            seen.cr <- false;
            seen.base <- true;
            Buffer.add_string buf (String.sub str i (stop - i));
            go stop
        | None ->
            text buf ~keep_newlines seen (Uchar.utf_decode_uchar d);
            go (i + Uchar.utf_decode_length d)
    end
  in
  go 0

let seen () = { cr = false; base = true }

(* [is_clean] is [true] iff [sanitize] returns [str] unchanged. Then [str] is
   valid UTF-8 and has no character to remove, no variation selector without a
   base, and no control character except the line feed, and that one only if
   line feeds are kept. *)
let rec is_clean ~keep_newlines str i base =
  i >= String.length str
  ||
  let d = String.get_utf_8_uchar str i in
  Uchar.utf_decode_is_valid d
  &&
  match tag_sequence str i d with
  | Some stop -> is_clean ~keep_newlines str stop true
  | None ->
      let u = Uchar.utf_decode_uchar d in
      let c = Uchar.to_int u in
      let next = i + Uchar.utf_decode_length d in
      if c = 0x0a then keep_newlines && is_clean ~keep_newlines str next false
      else if c < 0x20 || (c >= 0x7f && c <= 0x9f) then false
      else if c >= 0x80 && (removed u || (is_selector u && not base)) then false
      else
        is_clean ~keep_newlines str next
          (c > 0x20
          && (c < 0x80 || not (is_selector u))
          && c <> 0x200c && c <> 0x200d)

let rec ascii_from s i =
  i >= String.length s
  || (Char.Ascii.is_print (String.unsafe_get s i) && ascii_from s (i + 1))

let sanitize ?(keep_newlines = true) str =
  if ascii_from str 0 || is_clean ~keep_newlines str 0 true then str
  else
    let buf = Buffer.create (String.length str) in
    sanitize_into buf ~keep_newlines (seen ()) str;
    Buffer.contents buf

(* The construct that the escape scanner is inside. ECMA-48 (5th ed.) 5.3: a C1
   control is ESC Fe in a 7-bit code, with Fe in 04/00 to 05/15. In an 8-bit
   code it is 08/00 to 09/15, which UTF-8 writes as U+0080 to U+009F. *)
type state =
  | Ground
  | Escape  (** After ESC (ECMA-35 13.2). *)
  | Escape_intermediate  (** After ESC and an intermediate byte, 02/00-02/15. *)
  | Sequence  (** After CSI (ECMA-48 5.4). *)
  | String of { bel : bool }
      (** Inside a control string (ECMA-48 5.6). [bel] is [true] when BEL also
          closes it, as in xterm's OSC. *)

let esc = 0x1b
let bel = 0x07
let can = 0x18
let sub = 0x1a
let del = 0x7f

(* The controls that a terminal acts on without printing anything: DEL, and
   the C0 controls except HT (which leaves blank cells), the line ends (LF,
   VT, FF and CR) and ESC. *)
let silent c = (c < 0x20 && c <> esc && not (c >= 0x09 && c <= 0x0d)) || c = del
let is_c1 c = c >= 0x80 && c <= 0x9f
let nel = 0x85

(* The state that a C1 control [c] leads to. CSI opens a control sequence
   (8.3.16). OSC (8.3.89), DCS (8.3.27), SOS (8.3.128), PM (8.3.94) and APC
   (8.3.2) open a control string. Every other C1 control, including ST
   (8.3.143), is a complete control function. *)
let c1 = function
  | 0x9b -> Sequence
  | 0x9d -> String { bel = true }
  | 0x90 | 0x98 | 0x9e | 0x9f -> String { bel = false }
  | _ -> Ground

(* ECMA-35 13.2: ESC, intermediate bytes 02/00-02/15, then a final byte
   03/00-07/14. With a final byte in 04/00-05/15, the sequence is ESC Fe, a
   C1 control. *)
let after_escape c =
  if c >= 0x40 && c <= 0x5f then Some (c1 (c + 0x40))
  else if c >= 0x20 && c <= 0x2f then Some Escape_intermediate
  else if c >= 0x30 && c <= 0x7e then Some Ground
  else None

let after_intermediate c =
  if c >= 0x20 && c <= 0x2f then Some Escape_intermediate
  else if c >= 0x30 && c <= 0x7e then Some Ground
  else None

(* ECMA-48 5.4: parameter bytes 03/00-03/15 and intermediate bytes
   02/00-02/15, then the final byte, 04/00-07/14. *)
let in_sequence c =
  if c >= 0x20 && c <= 0x3f then Some Sequence
  else if c >= 0x40 && c <= 0x7e then Some Ground
  else None

let continues = function
  | Escape -> after_escape
  | Escape_intermediate -> after_intermediate
  | Ground | Sequence | String _ -> in_sequence

(* The effect of one character on the scan. [ends] is the tokens that it
   ends, as kind, start and stop. [next] is the next state. [at] is the offset
   of the next character to read. [from] is the start of the current token. *)
type move = {
  ends : ([ `Text | `Escape ] * int * int) list;
  next : state;
  at : int;
  from : int;
}

let move ?(ends = []) next at from = { ends; next; at; from }

(* NEL is text, because {!text} makes it a line end. A {!silent} control is
   an escape on its own. *)
let in_ground ~valid ~c ~i ~len ~start =
  if valid && silent c then
    move
      ~ends:[ (`Text, start, i); (`Escape, i, i + len) ]
      Ground (i + len) (i + len)
  else if valid && (c = esc || (is_c1 c && c <> nel)) then
    match if c = esc then Escape else c1 c with
    | Ground ->
        move
          ~ends:[ (`Text, start, i); (`Escape, i, i + len) ]
          Ground (i + len) (i + len)
    | next -> move ~ends:[ (`Text, start, i) ] next (i + len) i
  else move Ground (i + len) start

(* Inside an escape or a control sequence, the DEC parser that xterm follows
   acts on a C0 control and stays in the sequence. It ignores DEL. ESC, CAN,
   SUB and a C1 control end the sequence, then act. Any other character
   outside the ranges of the sequence ends it in error, and is read again as
   text. A control string ends at ST (ESC 05/12 or 09/12), and an OSC also
   ends at BEL. CAN, SUB, ESC and a C1 control end a control string, then act.
   A {!silent} control acts by being discarded. *)
let in_string str ~bel:b state ~valid ~c ~i ~len ~start =
  let n = String.length str in
  if not valid then move state (i + len) start
  else if c = esc && i + 1 < n && str.[i + 1] = '\\' then
    move ~ends:[ (`Escape, start, i + 2) ] Ground (i + 2) (i + 2)
  else if c = esc || c = can || c = sub || is_c1 c then
    move ~ends:[ (`Escape, start, i) ] Ground i i
  else if b && c = bel then
    move ~ends:[ (`Escape, start, i + len) ] Ground (i + len) (i + len)
  else move state (i + len) start

let in_sequence_or_escape state ~valid ~c ~i ~len ~start =
  if not valid then move ~ends:[ (`Escape, start, i) ] Ground i i
  else if c < 0x20 && c <> esc && c <> can && c <> sub then
    let kind = if silent c then `Escape else `Text in
    move
      ~ends:[ (`Escape, start, i); (kind, i, i + len) ]
      state (i + len) (i + len)
  else if c = del then move state (i + len) start
  else
    match continues state c with
    | Some Ground ->
        move ~ends:[ (`Escape, start, i + len) ] Ground (i + len) (i + len)
    | Some next -> move next (i + len) start
    | None -> move ~ends:[ (`Escape, start, i) ] Ground i i

(* [fold_tokens f acc str] folds [f] over the tokens of [str]. It also returns
   the start of the unfinished escape code at the end of [str], if any. *)
let fold_tokens f acc str =
  let n = String.length str in
  let emit acc (kind, start, stop) =
    if stop > start then f acc kind start (stop - start) else acc
  in
  let rec go acc i state start =
    if i >= n then
      if state = Ground then (emit acc (`Text, start, n), None)
      else (emit acc (`Escape, start, n), Some start)
    else
      let d = String.get_utf_8_uchar str i in
      let len = Uchar.utf_decode_length d in
      let valid = Uchar.utf_decode_is_valid d in
      let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let m =
        match state with
        | Ground -> in_ground ~valid ~c ~i ~len ~start
        | String { bel } -> in_string str ~bel state ~valid ~c ~i ~len ~start
        | Escape | Escape_intermediate | Sequence ->
            in_sequence_or_escape state ~valid ~c ~i ~len ~start
      in
      go (List.fold_left emit acc m.ends) m.at m.next m.from
  in
  go acc 0 Ground 0

(* ECMA-48 8.3.117 SELECT GRAPHIC RENDITION: CSI, parameter bytes, then the
   final byte 'm'. The accepted parameter bytes are the digits and ';', a
   strict subset of those of 5.4. At most 64 are accepted, which is more than
   any real SGR has. *)
let sgr seq =
  let n = String.length seq in
  let params_ok =
    n >= 3
    && n - 3 <= 64
    && seq.[0] = '\027'
    && seq.[1] = '['
    && seq.[n - 1] = 'm'
    && String.for_all
         (function '0' .. '9' | ';' -> true | _ -> false)
         (String.sub seq 2 (n - 3))
  in
  if not params_ok then None
  else
    (* Parameters apply from left to right. 0, or an empty parameter, turns
       every attribute off. 38, 48 and 58 read a colour from the parameters
       after them, [5;n] or [2;r;g;b]. These are values, not commands. If fewer
       parameters follow, all the remaining ones are values. *)
    let params = String.split_on_char ';' (String.sub seq 2 (n - 3)) in
    let rec state acc = function
      | [] -> acc
      | ("38" | "48" | "58") :: "5" :: _ :: rest -> state `Set rest
      | ("38" | "48" | "58") :: "2" :: _ :: _ :: _ :: rest -> state `Set rest
      | ("38" | "48" | "58") :: _ -> `Set
      | p :: rest ->
          let acc =
            if p = "" || int_of_string_opt p = Some 0 then `Reset else `Set
          in
          state acc rest
    in
    Some (state `Reset params)

let rewrite ~keep_newlines ~keep str =
  let buf = Buffer.create (String.length str) and seen = seen () in
  let open_style, _ =
    fold_tokens
      (fun open_style kind pos len ->
        let token = String.sub str pos len in
        match kind with
        | `Text ->
            sanitize_into buf ~keep_newlines seen token;
            open_style
        | `Escape when token = "\027E" ->
            sanitize_into buf ~keep_newlines seen "\u{0085}";
            open_style
        | `Escape -> (
            match keep token with
            | Some state ->
                Buffer.add_string buf token;
                state = `Set
            | None -> open_style))
      false str
  in
  if open_style then Buffer.add_string buf "\027[0m";
  Buffer.contents buf

let strip ?(keep_newlines = true) ?(keep_styles = false) str =
  rewrite ~keep_newlines ~keep:(if keep_styles then sgr else fun _ -> None) str

(* The length of [str], without its last character if that character is not
   valid UTF-8, because the bytes after [str] may complete it. *)

let character_end str =
  let n = String.length str in
  let rec last i =
    if i >= n then n
    else
      let d = String.get_utf_8_uchar str i in
      let next = i + Uchar.utf_decode_length d in
      if next < n then last next
      else if Uchar.utf_decode_is_valid d then n
      else i
  in
  last 0

let boundary str =
  match snd (fold_tokens (fun () _ _ _ -> ()) () str) with
  | Some start when start > 0 -> start
  | Some _ | None -> character_end str
