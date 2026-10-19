(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let to_string ?(styled = false) pp v =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  (* A string has no terminal, so a widget that fits its formatter's margin
     renders at its natural width. *)
  Format.pp_set_margin ppf max_int;
  Fmt.set_style_renderer ppf (if styled then `Ansi_tty else `None);
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let sanitize = Console_text.sanitize

(* ECMA-48 8.3.117 SELECT GRAPHIC RENDITION, inside the control sequence of
   ECMA-48 5.4: CSI, parameter bytes, final byte 'm'. The accepted parameter
   bytes are a strict subset of 5.4's -- the digits and ';' -- so a private
   parameter byte (5.4 reserves '<' to '?', which is how a terminal is told to
   hide its cursor or change a mode) and the ':' sub-parameter of ITU-T T.416
   are refused rather than forwarded on a guess. Every other escape sequence
   is refused too: cursor movement and erase share this grammar with a
   different final byte, and OSC, DCS and the C1 introducer do not share it at
   all. *)
let sgr_max_param_bytes = 64
(* An SGR setting both 24-bit colours and every attribute this library emits is
   under fifty bytes, so the bound never cuts a real sequence; what it bounds is
   the scan, which over untrusted bytes is otherwise sized by whoever wrote
   them. *)

(* [sgr_length s i] is the length of the SGR sequence starting at [i] in [s], or
   0 when what starts there is not one. *)
let sgr_length s i =
  let n = String.length s in
  if i + 1 >= n || s.[i] <> '\027' || s.[i + 1] <> '[' then 0
  else
    let rec scan j =
      if j >= n then 0
      else
        match s.[j] with
        | '0' .. '9' | ';' ->
            if j - (i + 2) >= sgr_max_param_bytes then 0 else scan (j + 1)
        | 'm' -> j + 1 - i
        | _ -> 0
    in
    scan (i + 2)

(* Whether [seq], an SGR sequence, leaves the terminal in its default state.
   Parameters apply left to right and 0 turns every attribute off, so what
   decides is the last one; an empty parameter is 0, and so is an empty list. *)
let sgr_closes_style seq =
  let params = String.sub seq 2 (String.length seq - 3) in
  let last =
    match String.rindex_opt params ';' with
    | None -> params
    | Some k -> String.sub params (k + 1) (String.length params - k - 1)
  in
  last = "" || int_of_string_opt last = Some 0

let style_is_open s =
  let n = String.length s in
  let rec scan i open_ =
    if i >= n then open_
    else
      match sgr_length s i with
      | 0 -> scan (i + 1) open_
      | len -> scan (i + len) (not (sgr_closes_style (String.sub s i len)))
  in
  scan 0 false

let sanitize_styles ?keep_newlines str =
  let n = String.length str in
  let buf = Buffer.create n in
  let plain first last =
    Buffer.add_string buf (sanitize ?keep_newlines (String.sub str first last))
  in
  (* The bytes an SGR sequence is made of are all below 0x80, so cutting the
     string at one cannot cut a UTF-8 sequence, and each run between two of them
     decodes on its own exactly as it would in place. *)
  let last_style = ref None in
  let rec loop start i =
    if i >= n then plain start (n - start)
    else
      let len = sgr_length str i in
      if len = 0 then loop start (i + 1)
      else begin
        plain start (i - start);
        let seq = String.sub str i len in
        Buffer.add_string buf seq;
        last_style := Some seq;
        loop (i + len) (i + len)
      end
  in
  loop 0 0;
  (* Styling inside the text is what this variant is for; styling of what the
     caller prints after it is the half of sanitize's promise that still holds,
     so a run left open is closed here. *)
  (match !last_style with
  | Some seq when not (sgr_closes_style seq) ->
      Buffer.add_string buf Ansi.reset_code
  | Some _ | None -> ());
  Buffer.contents buf

(* A terminal that has just filled its last column is in the pending-wrap
   state.  LF alone moves down but keeps that state/column on common VTs, so
   the next glyph may wrap once more.  CRLF is the unambiguous physical-row
   separator for ANSI output; plain formatter output keeps conventional LF. *)
let newline ppf =
  Fmt.string ppf (if Fmt.style_renderer ppf = `Ansi_tty then "\r\n" else "\n")
