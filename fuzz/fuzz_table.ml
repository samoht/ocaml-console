(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Cell text whose bytes, clusters and cells do not line up. *)
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

(* What [pp] prints at [margin], in colour, as lines without their escapes. *)
let render pp margin v =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Fmt.set_style_renderer ppf `Ansi_tty;
  pp ppf v;
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buf in
  (raw, List.map Console_text.strip (String.split_on_char (Char.chr 10) raw))

let cells s = Width.string_width s

let check_fits margin lines =
  List.iter
    (fun l ->
      if cells l > margin then
        failf "%d cells at margin %d: %S" (cells l) margin l)
    lines

(* Every style a widget turns on, it turns off before the line ends. *)
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
              || String.starts_with ~prefix:"\027]8;;\027" rest)
          then failf "style left on: %S" l)
    (String.split_on_char (Char.chr 10) raw)

let table s =
  let ncol = 1 + (Char.code (String.get (s ^ "\001") 0) mod 4) in
  let columns =
    List.init ncol (fun i ->
        Table.column
          ~align:(match i mod 3 with 0 -> `Left | 1 -> `Right | _ -> `Center)
          ~overflow:(if i mod 2 = 0 then `Wrap else `Truncate)
          (pick s (i + 1)))
  in
  let rows =
    List.init 3 (fun r ->
        List.init ncol (fun c -> Span.text (pick s (2 + (r * ncol) + c))))
  in
  (ncol, Table.of_rows ~border:Border.rounded columns rows)

(* No line is wider than the margin, every style is turned off, and a framed
   table that fits has all its lines as wide as one another. *)
let test_fits s n =
  let margin = 4 + (abs n mod 80) in
  let ncol, t = table s in
  let raw, lines = render Table.pp margin t in
  check_fits margin lines;
  check_closed raw;
  if margin >= (ncol * 3) + 1 then
    match lines with
    | first :: rest ->
        List.iter (fun l -> check_eq ~pp:Fmt.int (cells first) (cells l)) rest
    | [] -> fail "no line"

(* A right-aligned column's cells end in the same column. *)
let test_right_aligned s =
  let cells_of i = Span.text (pick s i) in
  let rows = List.init 4 (fun i -> [ cells_of i; Span.text "|" ]) in
  let t =
    Table.of_rows ~border:Border.none
      [ Table.column ~align:`Right "a"; Table.column "b" ]
      rows
  in
  let firsts = Span.text "a" :: List.map List.hd rows in
  let w = List.fold_left (fun n c -> max n (Span.width c)) 0 firsts in
  List.iter2
    (fun cell line ->
      let text = Span.to_string cell in
      if not (String.contains text '\n') then begin
        let expected = String.make (w - Span.width cell) ' ' ^ text in
        if not (String.starts_with ~prefix:expected line) then
          failf "%S does not start with %S" line expected
      end)
    firsts
    (String.split_on_char '\n' (Table.to_string t))

(* A row with more cells than columns, or none, is refused. *)
let test_shape s n =
  let k = abs n mod 4 in
  let rows = [ List.init k (fun i -> Span.text (pick s i)) ] in
  match Table.of_rows [ Table.column "a"; Table.column "b" ] rows with
  | _ -> if k = 0 || k > 2 then fail "bad row accepted"
  | exception Invalid_argument _ -> if k >= 1 && k <= 2 then fail "row refused"

let test_huge_min_width n =
  let minimum = max_int - (n mod 1024) in
  let t =
    Table.of_rows
      [ Table.column ~min_width:minimum "A"; Table.column "B" ]
      [ [ Span.text "x"; Span.text "y" ] ]
  in
  let out = Table.to_string ~width:20 t in
  check_fits 20 (String.split_on_char '\n' out)

let test_wide_border n =
  let glyph = String.make (2 + (n mod 3)) '#' in
  let border =
    Border.v
      Border.
        {
          top_left = glyph;
          top = glyph;
          top_right = glyph;
          left = glyph;
          right = glyph;
          bottom_left = glyph;
          bottom = glyph;
          bottom_right = glyph;
          cross = glyph;
          top_cross = glyph;
          bottom_cross = glyph;
          left_cross = glyph;
          right_cross = glyph;
        }
  in
  let t =
    Table.of_rows ~border
      [ Table.column "A"; Table.column "B" ]
      [ [ Span.text "x"; Span.text "y" ] ]
  in
  let lines = String.split_on_char '\n' (Table.to_string ~width:40 t) in
  check_fits 40 lines;
  let widths = List.map cells lines in
  List.iter (fun w -> check_eq ~pp:Fmt.int (List.hd widths) w) widths

let suite =
  ( "table",
    [
      test_case "multi-cell borders stay rectangular" [ uint8 ] test_wide_border;
      test_case "huge minimum narrows to available width" [ uint8 ]
        test_huge_min_width;
      test_case "fits the margin" [ bytes; int ] test_fits;
      test_case "right-aligned cells line up" [ bytes ] test_right_aligned;
      test_case "row shape" [ bytes; int ] test_shape;
    ] )
