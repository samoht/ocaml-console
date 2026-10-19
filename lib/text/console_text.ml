(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [text buf ~keep_newlines u] adds what [sanitize] makes of the character
   [u]: itself, or a space for a control. *)
let text buf ~keep_newlines u =
  let c = Uchar.to_int u in
  if (keep_newlines && c = 0x0a) || (c >= 0x20 && not (c >= 0x7f && c <= 0x9f))
  then Buffer.add_utf_8_uchar buf u
  else Buffer.add_char buf ' '

let sanitize_into buf ~keep_newlines str =
  let n = String.length str in
  let rec go i =
    if i < n then begin
      let d = String.get_utf_8_uchar str i in
      if Uchar.utf_decode_is_valid d then
        text buf ~keep_newlines (Uchar.utf_decode_uchar d)
      else Buffer.add_utf_8_uchar buf Uchar.rep;
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0

let sanitize ?(keep_newlines = true) str =
  let buf = Buffer.create (String.length str) in
  sanitize_into buf ~keep_newlines str;
  Buffer.contents buf

(* What the scan is inside of. ECMA-48 (5th ed.) 5.3: a C1 control is ESC Fe
   in a 7-bit code, Fe in 04/00 to 05/15, and 08/00 to 09/15 in an 8-bit code,
   which UTF-8 writes as U+0080 to U+009F. *)
type state =
  | Ground
  | Escape  (** After ESC (ECMA-35 13.2). *)
  | Escape_intermediate  (** After ESC and an intermediate byte, 02/00-02/15. *)
  | Sequence  (** After CSI (ECMA-48 5.4). *)
  | String of { bel : bool }
      (** Inside a control string (ECMA-48 5.6); [bel] when BEL closes it, as
          xterm's OSC. *)

let esc = 0x1b
let bel = 0x07
let can = 0x18
let sub = 0x1a
let del = 0x7f
let is_c1 c = c >= 0x80 && c <= 0x9f

(* The state a C1 control [c] leads to: CSI opens a control sequence (8.3.16);
   OSC (8.3.89), DCS (8.3.27), SOS (8.3.128), PM (8.3.94) and APC (8.3.2) open
   a control string; every other one, ST (8.3.143) among them, is a control
   function of its own. *)
let c1 = function
  | 0x9b -> Sequence
  | 0x9d -> String { bel = true }
  | 0x90 | 0x98 | 0x9e | 0x9f -> String { bel = false }
  | _ -> Ground

(* ECMA-35 13.2: ESC, intermediate bytes 02/00-02/15, a final byte
   03/00-07/14; a final byte in 04/00-05/15 is ESC Fe, a C1 control. *)
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

(* Inside an escape or a control sequence, the DEC parser xterm follows acts on
   a C0 control and stays, ignores DEL, and lets ESC, CAN, SUB and a C1 control
   end the sequence and act on their own; any other character outside the
   sequence's ranges ends it in error, and is text. A control string ends at
   ST, ESC 05/12 or 09/12, an OSC also at BEL; CAN, SUB, ESC and a C1 control
   end it and act. *)
(* What the character at [i], [len] bytes long, does from [state] in a token
   that began at [start]: the tokens it ends, and where the scan goes on. *)
type move = {
  ends : ([ `Text | `Escape ] * int * int) list;
  next : state;
  at : int;
  from : int;
}

let move ?(ends = []) next at from = { ends; next; at; from }

let in_ground ~valid ~c ~i ~len ~start =
  if valid && (c = esc || is_c1 c) then
    match if c = esc then Escape else c1 c with
    | Ground ->
        move
          ~ends:[ (`Text, start, i); (`Escape, i, i + len) ]
          Ground (i + len) (i + len)
    | next -> move ~ends:[ (`Text, start, i) ] next (i + len) i
  else move Ground (i + len) start

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
    move
      ~ends:[ (`Escape, start, i); (`Text, i, i + len) ]
      state (i + len) (i + len)
  else if c = del then move state (i + len) start
  else
    match continues state c with
    | Some Ground ->
        move ~ends:[ (`Escape, start, i + len) ] Ground (i + len) (i + len)
    | Some next -> move next (i + len) start
    | None -> move ~ends:[ (`Escape, start, i) ] Ground i i

let fold_tokens f acc str =
  let n = String.length str in
  let emit acc (kind, start, stop) =
    if stop > start then f acc kind start (stop - start) else acc
  in
  let rec go acc i state start =
    if i >= n then
      emit acc ((if state = Ground then `Text else `Escape), start, n)
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

(* ECMA-48 8.3.117 SELECT GRAPHIC RENDITION: CSI, parameter bytes, final byte
   'm'. The accepted parameter bytes are a strict subset of 5.4's -- the
   digits and ';' -- and at most 64 of them, which no real SGR reaches. *)
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
    (* Parameters apply left to right and 0, or an empty one, turns every
       attribute off. 38, 48 and 58 take a colour as the parameters after
       them, [5;n] or [2;r;g;b], which are values rather than commands. *)
    let params = String.split_on_char ';' (String.sub seq 2 (n - 3)) in
    let rec state acc = function
      | [] -> acc
      | ("38" | "48" | "58") :: "5" :: _ :: rest -> state `Set rest
      | ("38" | "48" | "58") :: "2" :: _ :: _ :: _ :: rest -> state `Set rest
      | p :: rest ->
          let acc =
            if p = "" || int_of_string_opt p = Some 0 then `Reset else `Set
          in
          state acc rest
    in
    Some (state `Reset params)

let rewrite ~keep_newlines ~keep str =
  let buf = Buffer.create (String.length str) in
  let open_style =
    fold_tokens
      (fun open_style kind pos len ->
        let token = String.sub str pos len in
        match kind with
        | `Text ->
            sanitize_into buf ~keep_newlines token;
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

let strip ?(keep_newlines = true) str =
  rewrite ~keep_newlines ~keep:(fun _ -> None) str

let sanitize_styles ?(keep_newlines = true) str =
  rewrite ~keep_newlines ~keep:sgr str
