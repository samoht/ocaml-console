(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Eighth-of-a-cell glyphs. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
let repeat n glyph = String.concat "" (List.init (max 0 n) (fun _ -> glyph))
let default_style = Style.fg Color.cyan

(* [n] cells of [glyph] in [style]; no cell adds nothing, not even a style. *)
let run style n glyph =
  if n <= 0 then Span.empty else Span.styled style (repeat n glyph)

let framed ~track ~brackets body =
  if brackets then
    Span.concat [ Span.styled track "▕"; body; Span.styled track "▏" ]
  else body

let check_width fn width =
  if width < 0 then invalid_arg ("Console.Bar." ^ fn ^ ": negative width")

let v ?(style = default_style) ?(track = Style.faint) ?(brackets = false) ~width
    frac =
  check_width "v" width;
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  let e = int_of_float (Float.round (frac *. float_of_int (width * 8))) in
  let full = e / 8 and part = e mod 8 in
  let fill =
    repeat full eighths.(8) ^ if part > 0 then eighths.(part) else ""
  in
  let used = full + if part > 0 then 1 else 0 in
  framed ~track ~brackets
    (Span.concat
       [
         (if fill = "" then Span.empty else Span.styled style fill);
         run track (width - used) "░";
       ])

let cells ~width ~scale x =
  if not (x > 0. && scale > 0.) then 0
  else
    let r = Float.min 1. (x /. scale) in
    max 1 (min width (int_of_float (Float.round (r *. float_of_int width))))

let spread ?(style = default_style) ?(track = Style.faint) ?(brackets = false)
    ~width ~scale median p95 =
  check_width "spread" width;
  let m = cells ~width ~scale median in
  let p = max m (cells ~width ~scale p95) in
  framed ~track ~brackets
    (Span.concat
       [ run style m "█"; run style (p - m) "▒"; run track (width - p) "░" ])

(* Largest remainder over integers: part [i] of [total] takes
   [v * width / total] cells, and the parts with the largest
   [v * width mod total], the earlier first on a tie, one more each until the
   cells add up to [width]. *)
let shares ~width counts =
  let total = List.fold_left ( + ) 0 counts in
  let floors = List.map (fun v -> v * width / total) counts in
  let left = width - List.fold_left ( + ) 0 floors in
  let extra =
    List.mapi (fun i v -> (i, v * width mod total)) counts
    |> List.stable_sort (fun (_, a) (_, b) -> Int.compare b a)
    |> List.filteri (fun k _ -> k < left)
    |> List.map fst
  in
  List.mapi (fun i f -> if List.mem i extra then f + 1 else f) floors

let stacked ?(track = Style.faint) ?(brackets = false) ~width parts =
  check_width "stacked" width;
  if List.exists (fun (n, _) -> n < 0) parts then
    invalid_arg "Console.Bar.stacked: negative count";
  let body =
    if List.for_all (fun (n, _) -> n = 0) parts then run track width "░"
    else
      Span.concat
        (List.map2
           (fun n (_, style) -> run style n "█")
           (shares ~width (List.map fst parts))
           parts)
  in
  framed ~track ~brackets body
