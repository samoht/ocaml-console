(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Text whose clusters, cells and bytes do not line up, over several
   lines. *)
let alphabet = [| "x"; "\xe7\x95\x8c"; "e\xcc\x81"; " "; "\n"; "ab"; "\n\n" |]

let text s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         alphabet.(Char.code s.[i] mod Array.length alphabet)))

let lines s = String.split_on_char '\n' s
let width = Width.string_width

(* Each line of [hcat] is the blocks' lines up to the last block with text
   on it, each but the last padded to its block's width and followed by
   the gap: its width is the widths and gaps before that block plus its
   line's, and no line is wider than the blocks' widths and gaps. *)
let test_hcat texts gap =
  let gap = gap mod 5 in
  let blocks = List.map (fun s -> lines (text s)) texts in
  let pp =
    hcat ~gap (List.map (fun s -> Fmt.const Span.pp (Span.text (text s))) texts)
  in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 10_000;
  pp ppf ();
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  let widths =
    List.map (List.fold_left (fun n l -> max n (width l)) 0) blocks
  in
  let height = List.fold_left (fun n b -> max n (List.length b)) 0 blocks in
  (* No line and one empty line print alike. *)
  let got = if out = "" && height = 0 then [] else lines out in
  if texts = [] then check_eq ~pp:Fmt.Dump.string "" out
  else begin
    check_eq ~pp:Fmt.int height (List.length got);
    let total =
      List.fold_left ( + ) 0 widths + (gap * (List.length blocks - 1))
    in
    List.iteri
      (fun i line ->
        let cell b = match List.nth_opt b i with Some l -> l | None -> "" in
        let last = ref (-1) in
        List.iteri (fun j b -> if width (cell b) > 0 then last := j) blocks;
        let before = ref 0 in
        List.iteri
          (fun j w -> if j < !last then before := !before + w + gap)
          widths;
        let want =
          if !last < 0 then 0
          else !before + width (cell (List.nth blocks !last))
        in
        check_eq ~pp:Fmt.int want (width line);
        if width line > total then failf "%S wider than %d" line total)
      got
  end

let suite =
  ( "console",
    [
      test_case "hcat lines are the blocks' widths and gaps"
        [ list bytes; uint8 ]
        test_hcat;
    ] )
