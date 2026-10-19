(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type overflow = [ `Wrap | `Truncate ]

type column = {
  header : Span.t;
  align : align;
  min_width : int option;
  max_width : int option;
  overflow : overflow;
  shrinkable : bool;
  style : Style.t;
}

(* A cell is kept as its lines, split once. *)
type t = {
  border : Border.t;
  header_style : Style.t;
  columns : column array;
  headers : Span.t list array;
  rows : Span.t list array list;
}

let column ?(align = `Left) ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  let negative = function Some w -> w < 0 | None -> false in
  if negative min_width then
    invalid_arg "Console.Table.column: negative min_width";
  if negative max_width then
    invalid_arg "Console.Table.column: negative max_width";
  (match (min_width, max_width) with
  | Some min, Some max when min > max ->
      invalid_arg "Console.Table.column: min_width exceeds max_width"
  | _ -> ());
  let header = Span.text header in
  { header; align; min_width; max_width; overflow; shrinkable; style }

let of_rows ?theme ?border ?(header_style = Style.bold) columns rows =
  let expected = List.length columns in
  let rows =
    List.map
      (fun cells ->
        let got = List.length cells in
        if got > expected || (got = 0 && expected > 0) then
          Fmt.invalid_arg
            "Console.Table.of_rows: expected 1 to %d cells, got %d" expected got;
        Array.of_list (List.map Span.lines cells))
      rows
  in
  let columns = Array.of_list columns in
  {
    border = Widget.border ?theme ?border ~default:Border.single ();
    header_style;
    columns;
    headers = Array.map (fun c -> Span.lines c.header) columns;
    rows;
  }

(* {1 Column widths} *)

let cells s = Span.width (Span.text s)
let chars t = Border.chars t.border

(* A border with no glyphs has no edge: no padding outside the first and the
   last cell, the last cell not padded out to its column, and a line ends at
   its last cell that is not empty. *)
let edged t = (chars t).left <> ""
let separator_width t = if edged t then 2 + cells (chars t).right else 2

let overhead t count =
  if count = 0 then 0
  else if not (edged t) then (count - 1) * 2
  else
    (count * 2) + cells (chars t).left + cells (chars t).right
    + ((count - 1) * cells (chars t).right)

let rendered_width t widths =
  Array.fold_left ( + ) (overhead t (Array.length widths)) widths

let lines_width = List.fold_left (fun n l -> max n (Span.width l)) 0

(* The widest word of a cell: the width a column keeps while another column
   can still give room, so that an identifier stays whole. *)
let word_width lines =
  List.fold_left (fun n line -> max n (Span.widest_word line)) 0 lines

(* A short row's last cell spans the columns it leaves and is measured
   against none of them. *)
let measured num_cols row =
  let n = Array.length row in
  if n >= num_cols then n else n - 1

let widen measure t widths =
  let num_cols = Array.length widths in
  Array.iteri (fun i h -> widths.(i) <- max widths.(i) (measure h)) t.headers;
  List.iter
    (fun row ->
      for i = 0 to measured num_cols row - 1 do
        widths.(i) <- max widths.(i) (measure row.(i))
      done)
    t.rows

(* Columns narrow, the widest first, until the table fits [max_width]: the
   shrinkable ones down to their widest word, then all of them, then down to
   their minimum, then to nothing. *)
let shrink ~max_width t words widths =
  let cols = t.columns in
  let min_of i = Option.value cols.(i).min_width ~default:1 in
  let word_min i =
    let token =
      match cols.(i).overflow with `Truncate -> 0 | `Wrap -> words.(i)
    in
    min widths.(i) (max (min_of i) token)
  in
  let pass can floor =
    let stuck = ref false in
    while rendered_width t widths > max_width && not !stuck do
      let widest = ref (-1) in
      Array.iteri
        (fun i w ->
          if can i && w > floor i && (!widest < 0 || w > widths.(!widest)) then
            widest := i)
        widths;
      if !widest < 0 then stuck := true
      else
        let i = !widest in
        let excess = rendered_width t widths - max_width in
        widths.(i) <- widths.(i) - min excess (widths.(i) - floor i)
    done
  in
  pass (fun i -> cols.(i).shrinkable) word_min;
  pass (fun _ -> true) word_min;
  pass (fun i -> cols.(i).shrinkable) min_of;
  pass (fun _ -> true) min_of;
  pass (fun _ -> true) (fun _ -> 0)

let widths ?max_width t =
  let n = Array.length t.columns in
  let widths = Array.make n 0 and words = Array.make n 0 in
  widen lines_width t widths;
  widen word_width t words;
  Array.iteri
    (fun i col ->
      Option.iter (fun m -> widths.(i) <- max widths.(i) m) col.min_width;
      Option.iter (fun m -> widths.(i) <- min widths.(i) m) col.max_width)
    t.columns;
  Option.iter (fun max_width -> shrink ~max_width t words widths) max_width;
  widths

(* {1 Lines} *)

let repeat n glyph =
  if n <= 0 || glyph = "" then ""
  else String.concat "" (List.init n (Fun.const glyph))

let rule t widths ~left ~mid ~right ~line =
  if (chars t).top = "" then []
  else
    let style = Border.style t.border in
    let last = Array.length widths - 1 in
    let parts =
      Array.to_list
        (Array.mapi
           (fun i w -> repeat (w + 2) line ^ if i < last then mid else "")
           widths)
    in
    [ Span.styled style (String.concat "" ((left :: parts) @ [ right ])) ]

let pad n = Span.text (String.make (max 0 n) ' ')

let align_span ~pad_right align target span =
  let padding = target - Span.width span in
  if padding <= 0 then span
  else
    let left, right =
      match align with
      | `Left -> (0, padding)
      | `Right -> (padding, 0)
      | `Center -> (padding / 2, padding - (padding / 2))
    in
    Span.(pad left ++ span ++ if pad_right then pad right else empty)

(* The physical lines of a cell in [width] cells; [None] is a cell nothing
   bounds, the last of a line without edges and no width. *)
let fit overflow width lines =
  match width with
  | None -> lines
  | Some width when width <= 0 -> [ Span.empty ]
  | Some width -> (
      match overflow with
      | `Wrap -> List.concat_map (Span.wrap width) lines
      | `Truncate -> List.map (Span.truncate width) lines)

(* The width each cell of [row] fills. A short row's last cell fills the
   columns it spans and the separators between them, and in a table without
   edges runs to the end of the line: to [max_width], or unbounded. *)
let slot_width ?max_width t widths row i =
  let num_cols = Array.length widths and n = Array.length row in
  if i < n - 1 || n = num_cols then Some widths.(i)
  else if edged t then begin
    let spanned = ref 0 in
    for j = i to num_cols - 1 do
      spanned := !spanned + widths.(j)
    done;
    Some (!spanned + ((num_cols - 1 - i) * separator_width t))
  end
  else
    let before = ref 0 in
    for j = 0 to i - 1 do
      before := !before + widths.(j) + 2
    done;
    Option.map (fun w -> max 0 (w - !before)) max_width

let row_lines ?max_width t widths style row =
  let cells =
    Array.mapi
      (fun i lines ->
        let col = t.columns.(i) in
        let width = slot_width ?max_width t widths row i in
        let lines = Array.of_list (fit col.overflow width lines) in
        (col, Option.value width ~default:0, lines))
      row
  in
  let height =
    Array.fold_left (fun h (_, _, l) -> max h (Array.length l)) 0 cells
  in
  let at lines k = if k < Array.length lines then lines.(k) else Span.empty in
  let cell_style col = Style.(style + col.style) in
  let chars = chars t and bstyle = Border.style t.border in
  let edge s = Span.styled bstyle s in
  List.init height (fun k ->
      if edged t then
        let last = Array.length cells - 1 in
        let parts =
          Array.to_list
            (Array.mapi
               (fun i (col, w, ls) ->
                 Span.(
                   text " "
                   ++ with_style (cell_style col)
                        (align_span ~pad_right:true col.align w (at ls k))
                   ++ text " "
                   ++ if i < last then edge chars.right else empty))
               cells)
        in
        Span.concat ((edge chars.left :: parts) @ [ edge chars.right ])
      else
        (* A line ends at its last cell that is not empty, unpadded. *)
        let shown = ref 0 in
        Array.iteri
          (fun i (_, _, lines) ->
            if Span.width (at lines k) > 0 then shown := i)
          cells;
        let parts =
          List.filteri (fun i _ -> i <= !shown) (Array.to_list cells)
          |> List.mapi (fun i (col, w, ls) ->
              Span.(
                (if i > 0 then text "  " else empty)
                ++ with_style (cell_style col)
                     (align_span ~pad_right:(i < !shown) col.align w (at ls k))))
        in
        Span.concat
          (parts @ if chars.right <> "" then [ edge chars.right ] else []))

let lines ?max_width t =
  if Array.length t.columns = 0 then []
  else
    let c = chars t in
    let widths = widths ?max_width t in
    let row style r = row_lines ?max_width t widths style r in
    let has_headers = Array.exists (fun h -> lines_width h > 0) t.headers in
    let body = List.concat_map (row Style.none) t.rows in
    let lines =
      rule t widths ~left:c.top_left ~mid:c.top_cross ~right:c.top_right
        ~line:c.top
      @ (if has_headers then
           row t.header_style t.headers
           @
           if t.rows <> [] then
             rule t widths ~left:c.left_cross ~mid:c.cross ~right:c.right_cross
               ~line:c.top
           else []
         else [])
      @ body
      @ rule t widths ~left:c.bottom_left ~mid:c.bottom_cross
          ~right:c.bottom_right ~line:c.bottom
    in
    (* Columns at their narrowest may still not fit beside the border: each
       line is then cut, visibly, rather than a column left out. *)
    match max_width with
    | None -> lines
    | Some w -> List.map (Span.truncate w) lines

let pp ppf t =
  Span.pp_lines ppf (lines ~max_width:(Format.pp_get_margin ppf ()) t)

let to_string ?styled ?width t =
  Span.lines_to_string ?styled (lines ?max_width:width t)
