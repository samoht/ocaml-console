(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [text buf ~keep_newlines u] adds what [sanitize] makes of the character
   [u]: itself, or a space for a control. *)
let text buf ~keep_newlines u =
  let c = Uchar.to_int u in
  if (keep_newlines && c = 0x0a) || (c >= 0x20 && not (c >= 0x7f && c <= 0x9f))
  then Uutf.Buffer.add_utf_8 buf u
  else Buffer.add_char buf ' '

let decode str f =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        f (Some u);
        loop ()
    | `Malformed _ ->
        f None;
        loop ()
    | `End -> ()
    | `Await -> assert false
  in
  loop ()

let sanitize ?(keep_newlines = true) str =
  let buf = Buffer.create (String.length str) in
  decode str (function
    | Some u -> text buf ~keep_newlines u
    | None -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep);
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

(* The state a C1 control [c] leads to: CSI opens a control sequence (8.3.16);
   OSC (8.3.89), DCS (8.3.27), SOS (8.3.128), PM (8.3.94) and APC (8.3.2) open
   a control string; every other one, ST (8.3.143) among them, is a control
   function of its own and is gone. *)
let c1 = function
  | 0x9b -> Sequence
  | 0x9d -> String { bel = true }
  | 0x90 | 0x98 | 0x9e | 0x9f -> String { bel = false }
  | _ -> Ground

let ground buf ~keep_newlines u =
  let c = Uchar.to_int u in
  if c = esc then Escape
  else if c >= 0x80 && c <= 0x9f then c1 c
  else (
    text buf ~keep_newlines u;
    Ground)

(* Inside an escape or a control sequence, the DEC parser xterm follows acts on
   a C0 control and stays, ignores DEL, and lets ESC, CAN, SUB and a C1 control
   end the sequence and act on their own. Any other character outside the
   sequence's ranges ends it in error, and is text. *)
let inside buf ~keep_newlines state ~continues u =
  let c = Uchar.to_int u in
  if c < 0x20 && c <> esc && c <> can && c <> sub then (
    text buf ~keep_newlines u;
    state)
  else if c = del then state
  else
    match continues c with Some s -> s | None -> ground buf ~keep_newlines u

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

(* ST is ESC 05/12 or 09/12: an ESC ends the string and opens an escape, which
   ST's second half closes; CAN and SUB cancel it, and a C1 control ends it and
   acts. Everything else, whatever 5.6 allows the string to hold, is the
   string's. *)
let in_string buf ~keep_newlines ~bel:b state u =
  let c = Uchar.to_int u in
  if c = esc || c = can || c = sub || (c >= 0x80 && c <= 0x9f) then
    ground buf ~keep_newlines u
  else if b && c = bel then Ground
  else state

let step buf ~keep_newlines state = function
  | None -> (
      match state with
      | String _ -> state
      | Ground | Escape | Escape_intermediate | Sequence ->
          Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
          Ground)
  | Some u -> (
      match state with
      | Ground -> ground buf ~keep_newlines u
      | Escape -> inside buf ~keep_newlines state ~continues:after_escape u
      | Escape_intermediate ->
          inside buf ~keep_newlines state ~continues:after_intermediate u
      | Sequence -> inside buf ~keep_newlines state ~continues:in_sequence u
      | String { bel } -> in_string buf ~keep_newlines ~bel state u)

let strip ?(keep_newlines = true) str =
  let buf = Buffer.create (String.length str) in
  let state = ref Ground in
  decode str (fun d -> state := step buf ~keep_newlines !state d);
  Buffer.contents buf
