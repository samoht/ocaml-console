(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Color = Color
module Style = Style
module Span = Span
module Border = Border
module Guide = Guide
module Spinner = Spinner
module Theme = Theme
module Bar = Bar
module Panel = Panel
module Table = Table
module Tree = Tree
module Rule = Rule
module Display = Display

let width lines = Array.fold_left (fun n l -> max n (Span_core.width l)) 0 lines

(* A line ends at its last block with text on it, a mark that takes no cell
   included: the blocks before it are padded to their widths and followed by
   the gap, and nothing follows. *)
let row ~gap blocks i =
  let cell (b, _) = if i < Array.length b then b.(i) else Span_core.empty in
  let last = ref (-1) in
  Array.iteri
    (fun j b -> if not (Span_core.is_empty (cell b)) then last := j)
    blocks;
  Span_core.concat
    (List.init (!last + 1) (fun j ->
         let ((_, w) as b) = blocks.(j) in
         let c = cell b in
         if j = !last then c
         else Span_core.(c ++ Widget.pad (w - Span_core.width c + gap))))

let hcat ?(gap = 2) blocks =
  if gap < 0 then invalid_arg "Console.hcat: negative gap";
  fun ppf () ->
    if blocks <> [] then begin
      let look = Look.v ppf and n = List.length blocks in
      let room = Format.pp_get_margin ppf () in
      let margin = max 1 ((room - (gap * (n - 1))) / n) in
      let block pp =
        let lines =
          Array.of_list (Span_core.capture ~look ~margin (fun ppf -> pp ppf ()))
        in
        (lines, width lines)
      in
      let blocks = Array.of_list (List.map block blocks) in
      let height =
        Array.fold_left (fun h (b, _) -> max h (Array.length b)) 0 blocks
      in
      (* The gaps may leave the blocks less than a cell each. *)
      let fit line = Widget.truncate look (max 1 room) line in
      Span_core.pp_lines ppf
        (List.init height (fun i -> fit (row ~gap blocks i)))
    end
