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

(* The index after the escape sequence at index [i] of [l], and its effect: it
   turns a style on or off, opens or closes a link, or neither. An OSC ends at
   BEL or at ESC \. *)
let escape l i =
  let n = String.length l in
  let rec upto p j = if j >= n || p l.[j] then j else upto p (j + 1) in
  if i + 1 < n && l.[i + 1] = '[' then
    let f = upto (fun c -> c >= '@' && c <= '~') (i + 2) in
    let params = String.sub l (i + 2) (f - i - 2) in
    if f < n && l.[f] = 'm' then (f + 1, `Style (params <> "" && params <> "0"))
    else (min n (f + 1), `Other)
  else if i + 1 < n && l.[i + 1] = ']' then
    let e = upto (fun c -> c = '\007' || c = '\027') (i + 2) in
    let next = min n (if e < n && l.[e] = '\027' then e + 2 else e + 1) in
    match String.split_on_char ';' (String.sub l (i + 2) (e - i - 2)) with
    | "8" :: _ :: uri -> (next, `Link (String.concat ";" uri <> ""))
    | _ -> (next, `Other)
  else (min n (i + 2), `Other)

let check_line l =
  let rec go i ~style ~link =
    match String.index_from_opt l i '\027' with
    | None ->
        if style then Alcobar.failf "style left on: %S" l;
        if link then Alcobar.failf "link left open: %S" l
    | Some i -> (
        match escape l i with
        | next, `Style style -> go next ~style ~link
        | next, `Link link -> go next ~style ~link
        | next, `Other -> go next ~style ~link)
  in
  go 0 ~style:false ~link:false

let check_closed raw = List.iter check_line (String.split_on_char '\n' raw)
