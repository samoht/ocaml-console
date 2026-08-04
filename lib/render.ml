(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let to_string ?(style_renderer = `None) pp v =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf style_renderer;
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let sanitize ?(keep_newlines = true) str =
  let buf = Buffer.create (String.length str) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        let c = Uchar.to_int u in
        if
          (keep_newlines && c = 0x0a)
          || (c >= 0x20 && not (c >= 0x7f && c <= 0x9f))
        then Uutf.Buffer.add_utf_8 buf u
        else Buffer.add_char buf ' ';
        loop ()
    | `Malformed _ ->
        Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
        loop ()
    | `End -> Buffer.contents buf
    | `Await -> assert false
  in
  loop ()

(* A terminal that has just filled its last column is in the pending-wrap
   state.  LF alone moves down but keeps that state/column on common VTs, so
   the next glyph may wrap once more.  CRLF is the unambiguous physical-row
   separator for ANSI output; plain formatter output keeps conventional LF. *)
let newline ppf =
  Fmt.string ppf (if Fmt.style_renderer ppf = `Ansi_tty then "\r\n" else "\n")
