(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let pool =
  [|
    "x";
    "abc";
    "\xe7\x95\x8c\xe7\x95\x8c";
    "e\xcc\x81";
    "\xf0\x9f\x8e\x89 ok";
    "a b c d e f";
    "src/lib/a.ml";
    "\xff\xfe";
    "\t\007bell";
    "";
    "a long cell that will not fit in a narrow column at all";
  |]

let pick s i =
  let s = s ^ "\000" in
  pool.((Char.code s.[i mod String.length s] + i) mod Array.length pool)

let spell alphabet s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         alphabet.(Char.code s.[i] mod Array.length alphabet)))

let room_width n = 1 + (n mod 200)
let nth_theme k = snd (List.nth Theme.all (k mod List.length Theme.all))

let render ?theme pp margin v =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Option.iter (Theme.set ppf) theme;
  Format.pp_set_margin ppf margin;
  Fmt.set_style_renderer ppf `Ansi_tty;
  pp ppf v;
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buf in
  (raw, List.map Console_text.strip (String.split_on_char (Char.chr 10) raw))

let cells s = Console_width.string_width s

let check_fits margin lines =
  List.iter
    (fun l ->
      if cells l > margin then
        Alcobar.failf "%d cells at margin %d: %S" (cells l) margin l)
    lines

let check_closed raw =
  List.iter
    (fun l ->
      match String.rindex_opt l (Char.chr 27) with
      | None -> ()
      | Some i ->
          let rest = String.sub l i (String.length l - i) in
          if
            not
              (String.starts_with ~prefix:"\027[0m" rest
              || String.ends_with ~suffix:"\027]8;;\027\\" l)
          then Alcobar.failf "style left on: %S" l)
    (String.split_on_char (Char.chr 10) raw)
