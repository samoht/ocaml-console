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
module Input = Input

let width lines = List.fold_left (fun n l -> max n (Span_core.width l)) 0 lines

(* A line ends at its last block with text on it: the blocks before it are
   padded to their widths and followed by the gap, and nothing follows. *)
let row ~gap blocks i =
  let cell (b, _) = Option.value (List.nth_opt b i) ~default:Span_core.empty in
  let last = ref (-1) in
  List.iteri (fun j b -> if Span_core.width (cell b) > 0 then last := j) blocks;
  Span_core.concat
    (List.filteri (fun j _ -> j <= !last) blocks
    |> List.mapi (fun j ((_, w) as b) ->
        let c = cell b in
        if j = !last then c
        else Span_core.(c ++ Widget.pad (w - Span_core.width c + gap))))

let hcat ?(gap = 2) blocks =
  if gap < 0 then invalid_arg "Console.hcat: negative gap";
  fun ppf () ->
    if blocks <> [] then begin
      let look = Look.v ppf and n = List.length blocks in
      let margin =
        max 1 ((Format.pp_get_margin ppf () - (gap * (n - 1))) / n)
      in
      let blocks =
        List.map
          (fun pp ->
            let lines =
              Span_core.capture ~look ~margin (fun ppf -> pp ppf ())
            in
            (lines, width lines))
          blocks
      in
      let height =
        List.fold_left (fun h (b, _) -> max h (List.length b)) 0 blocks
      in
      Span_core.pp_lines ppf (List.init height (row ~gap blocks))
    end
