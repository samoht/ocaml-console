(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let to_string pp ?(styled = false) v =
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
let sanitize_styles = Console_text.sanitize_styles
