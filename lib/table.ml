(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type overflow = [ `Truncate | `Wrap | `Elide ]

type column = {
  header : Span.t;
  align : align;
  min_width : int option;
  max_width : int option;
  overflow : overflow;
  shrinkable : bool;
  style : Style.t;
}

type row = { cells : Span.t list; see : string option }

type t = {
  border : Border.t;
  row_separators : bool;
  header_style : Style.t;
  columns : column list;
  rows : row list;
}

let check_widths min_width max_width =
  (match min_width with
  | Some width when width < 0 ->
      invalid_arg "Console.Table.column: negative min_width"
  | _ -> ());
  (match max_width with
  | Some width when width < 0 ->
      invalid_arg "Console.Table.column: negative max_width"
  | _ -> ());
  match (min_width, max_width) with
  | Some min, Some max when min > max ->
      invalid_arg "Console.Table.column: min_width exceeds max_width"
  | _ -> ()

let column ?(align = `Left) ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  check_widths min_width max_width;
  let header = Span.text header in
  { header; align; min_width; max_width; overflow; shrinkable; style }

let v ?theme ?border ?(header_style = Style.bold) columns =
  {
    border = Widget.border ?theme ?border ~default:Border.single ();
    row_separators =
      Option.fold ~none:false ~some:Theme.table_row_separators theme;
    header_style;
    columns;
    rows = [];
  }

let add_row ?see cells t =
  let expected = List.length t.columns and got = List.length cells in
  if got > expected || (got = 0 && expected > 0) then
    Fmt.invalid_arg "Console.Table.add_row: expected 1 to %d cells, got %d"
      expected got;
  { t with rows = { cells; see } :: t.rows }

let of_rows ?theme ?border ?header_style columns rows =
  let t = v ?theme ?border ?header_style columns in
  List.fold_left (fun t row -> add_row row t) t rows

(* Calculate column widths. A logical cell may contain explicit line feeds;
   its natural width is its widest physical line, not the sum of all lines. *)
let span_lines span =
  span |> Span.sanitize ~keep_newlines:true |> Span.split_lines

let span_width span =
  span_lines span
  |> List.fold_left (fun width line -> max width (Span.width line)) 0

(* The cells of [row] its columns are measured by: a short row's last cell
   spans the columns it leaves and is measured against none of them. *)
let measured num_cols row =
  let n = List.length row.cells in
  if n >= num_cols then row.cells
  else List.filteri (fun i _ -> i < n - 1) row.cells

(* [widen measure t widths] raises each column's entry in [widths] to the
   [measure] of its header and of each of its measured cells. *)
let widen measure t widths =
  let num_cols = Array.length widths in
  let widen_to i span = widths.(i) <- max widths.(i) (measure span) in
  List.iteri (fun i col -> widen_to i col.header) t.columns;
  List.iter
    (fun row ->
      List.iteri
        (fun i cell -> if i < num_cols then widen_to i cell)
        (measured num_cols row))
    t.rows

(* A whitespace-delimited token is kept intact while any column still has a
   cheaper whitespace break available. This makes a URL or identifier remain
   selectable without making it an absolute geometry escape hatch: the hard
   terminal width still wins when every such break has been exhausted. Slashes
   are intentionally part of the token here even though [Span.wrap] may use
   them as the final fallback. *)
let span_unbroken_width span =
  let word_widths line =
    Span.to_string line |> String.split_on_char ' '
    |> List.map Width.string_width
  in
  span_lines span |> List.concat_map word_widths |> List.fold_left max 0

let apply_width_constraints t widths =
  List.iteri
    (fun i col ->
      (match col.min_width with
      | Some min -> widths.(i) <- max widths.(i) min
      | None -> ());
      match col.max_width with
      | Some max -> widths.(i) <- min widths.(i) max
      | None -> ())
    t.columns

(* A border with no glyphs has no edge: no padding outside the first and the
   last cell, the last cell not padded out to its column, and a line ends at
   its last cell that is not empty. *)
let edged t = (Border.chars t.border).left <> ""

let separator_width t =
  if edged t then 2 + Width.string_width (Border.chars t.border).right else 2

let structural_overhead t count =
  if count = 0 then 0
  else
    let chars = Border.chars t.border in
    if not (edged t) then (count - 1) * 2
    else
      (count * 2)
      + Width.string_width chars.left
      + Width.string_width chars.right
      + ((count - 1) * Width.string_width chars.right)

let rendered_width t widths =
  Array.fold_left ( + ) (structural_overhead t (Array.length widths)) widths

(* Shrink columns to fit [max_width] columns when given. With no width to fit --
   a pipe or file -- squeezing would wrap or clip cell text (e.g. a long event
   name to "sp"), corrupting output meant to be read or machine-ingested, so
   the natural widths are kept. *)
let shrink_widths_to_terminal ?max_width t unbroken_widths widths =
  match max_width with
  | None -> widths
  | Some term_w ->
      let cols = Array.of_list t.columns in
      let min_of i = match cols.(i).min_width with Some m -> m | None -> 1 in
      let semantic_min i =
        let token =
          match cols.(i).overflow with
          | `Truncate | `Elide -> 0
          | `Wrap -> unbroken_widths.(i)
        in
        min widths.(i) (max (min_of i) token)
      in
      let shrink_while can_shrink floor =
        let done_ = ref false in
        while rendered_width t widths > term_w && not !done_ do
          let max_i = ref (-1) in
          let max_w = ref (-1) in
          Array.iteri
            (fun i w ->
              if can_shrink i && w > !max_w then (
                max_w := w;
                max_i := i))
            widths;
          if !max_i < 0 then done_ := true
          else
            let excess = rendered_width t widths - term_w in
            let lower = floor !max_i in
            let shrink = min excess (widths.(!max_i) - lower) in
            widths.(!max_i) <- widths.(!max_i) - shrink
        done
      in
      (* Preferences choose which columns yield first. They are deliberately
         not escape hatches from the geometry contract: when the preferred
         pass is exhausted, every column may shrink to zero before a terminal
         row is allowed to exceed [term_w]. *)
      shrink_while
        (fun i -> cols.(i).shrinkable && widths.(i) > semantic_min i)
        semantic_min;
      shrink_while (fun i -> widths.(i) > semantic_min i) semantic_min;
      shrink_while
        (fun i -> cols.(i).shrinkable && widths.(i) > min_of i)
        min_of;
      shrink_while (fun i -> widths.(i) > min_of i) min_of;
      shrink_while (fun i -> widths.(i) > 0) (fun _ -> 0);
      widths

let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

(* A border and cell padding need a minimum number of cells even when every
   value is hidden. At extremely narrow widths, keep the leading columns that
   structurally fit. This is preferable to silently wrapping every right edge
   onto a second terminal row. *)
let fit_columns max_width t =
  let rec count_fit count =
    if count >= List.length t.columns then count
    else if structural_overhead t (count + 1) <= max_width then
      count_fit (count + 1)
    else count
  in
  let count = count_fit 0 in
  if count = List.length t.columns then t
  else
    {
      t with
      columns = take count t.columns;
      rows =
        List.filter_map
          (fun r ->
            match take count r.cells with
            | [] -> None
            | cells -> Some { r with cells })
          t.rows;
    }

let calc_col_widths ?max_width t =
  let widths = Array.make (List.length t.columns) 0 in
  let unbroken_widths = Array.make (List.length t.columns) 0 in
  widen span_width t widths;
  widen span_unbroken_width t unbroken_widths;
  apply_width_constraints t widths;
  shrink_widths_to_terminal ?max_width t unbroken_widths widths

(* A rendered line, in pieces: border glyphs, inked in the border's style;
   padding between cells; and a cell's text with the style composed under
   it. *)
type piece = Edge of string | Gap of string | Cell of Style.t * Span.t

let horizontal t widths left_char mid_char right_char horiz_char =
  let chars = Border.chars t.border in
  if chars.top = "" then None
  else
    let last = Array.length widths - 1 in
    let line =
      Edge left_char
      :: List.concat
           (List.mapi
              (fun i col_width ->
                Edge
                  (String.concat ""
                     (List.init (col_width + 2) (fun _ -> horiz_char)))
                :: (if i < last then [ Edge mid_char ] else []))
              (Array.to_list widths))
      @ [ Edge right_char ]
    in
    Some line

let align_span ~pad_right align target span =
  let width = Span.width span in
  if width >= target then span
  else
    let padding = target - width in
    let left, right =
      match align with
      | `Left -> (0, padding)
      | `Right -> (padding, 0)
      | `Center -> (padding / 2, padding - (padding / 2))
    in
    let right = if pad_right then right else 0 in
    Span.(text (String.make left ' ') ++ span ++ text (String.make right ' '))

(* [span] cut by [Width.elide] to [width], the style of what it keeps kept:
   what [elide] keeps is a prefix of the text, and what follows it is plain. *)
let elide_span ?see width span =
  if Span.width span <= width then span
  else
    let s = Span.to_string span in
    let e = Width.elide ?see width s in
    let n = min (String.length s) (String.length e) in
    let rec common i = if i < n && s.[i] = e.[i] then common (i + 1) else i in
    let rec boundary i =
      if i > 0 && i < String.length s && Char.code s.[i] land 0xC0 = 0x80 then
        boundary (i - 1)
      else i
    in
    let p = boundary (common 0) in
    Span.(
      truncate (Width.string_width (String.sub s 0 p)) span
      ++ text (String.sub e p (String.length e - p)))

(* The physical lines of a cell in [width] columns, not padded; [None] is a
   cell nothing bounds, the last of a line without edges and no width. *)
let cell_lines ?see overflow width span =
  let fit line =
    match width with
    | None -> [ line ]
    | Some width when width <= 0 -> [ Span.empty ]
    | Some width -> (
        match overflow with
        | `Truncate -> (
            let cell_width = Span.width line in
            if cell_width <= width then [ line ]
            else
              (* Whole words from the start, never a word cut; a value whose
                 first word does not fit is printed whole, over several
                 lines. *)
              match Width.shorten width (Span.to_string line) with
              | "" -> Span.wrap width line
              | kept -> [ Span.truncate (Width.string_width kept) line ])
        | `Wrap -> Span.wrap width line
        | `Elide -> [ elide_span ?see width line ])
  in
  span_lines span |> List.concat_map fit

(* Each cell of [cells] with its column and the width it fills. A short row's
   last cell fills the columns it spans and the separators between them, and
   in a table without edges runs to the end of the line: to [max_width], or
   unbounded. *)
let slots ?max_width t widths cells =
  let columns = Array.of_list t.columns in
  let num_cols = Array.length columns in
  let n = List.length cells in
  List.mapi
    (fun i cell ->
      let width =
        if i < n - 1 || n = num_cols then Some widths.(i)
        else if edged t then (
          let spanned = ref 0 in
          for j = i to num_cols - 1 do
            spanned := !spanned + widths.(j)
          done;
          Some (!spanned + ((num_cols - 1 - i) * separator_width t)))
        else
          let before = ref 0 in
          for j = 0 to i - 1 do
            before := !before + widths.(j) + 2
          done;
          Option.map (fun w -> max 0 (w - !before)) max_width
      in
      (columns.(i), width, cell))
    cells

(* One physical line of a table with edges: every cell padded, between
   separators. *)
let edged_line t style line =
  let chars = Border.chars t.border in
  let last = List.length line - 1 in
  let pieces =
    List.concat
      (List.mapi
         (fun i (col, width, text) ->
           let text = Option.value ~default:Span.empty text in
           let target = Option.value ~default:0 width in
           [
             Gap " ";
             Cell (style col, align_span ~pad_right:true col.align target text);
             Gap " ";
           ]
           @ if i < last then [ Edge chars.right ] else [])
         line)
  in
  (Edge chars.left :: pieces) @ [ Edge chars.right ]

(* One physical line of a table without edges: it ends at its last cell that
   is not empty, unpadded. *)
let open_line t style line =
  let chars = Border.chars t.border in
  let shown =
    let rec upto i acc = function
      | [] -> acc
      | (_, _, Some text) :: rest when Span.to_string text <> "" ->
          upto (i + 1) i rest
      | _ :: rest -> upto (i + 1) acc rest
    in
    upto 0 0 line
  in
  let pieces =
    List.concat
      (List.mapi
         (fun i (col, width, text) ->
           if i > shown then []
           else
             let text = Option.value ~default:Span.empty text in
             let target = Option.value ~default:0 width in
             (if i > 0 then [ Gap "  " ] else [])
             @ [
                 Cell
                   ( style col,
                     align_span ~pad_right:(i < shown) col.align target text );
               ])
         line)
  in
  if chars.right <> "" then pieces @ [ Edge chars.right ] else pieces

let row_lines ?max_width t widths style row =
  let cells =
    List.map
      (fun (col, width, cell) ->
        (col, width, cell_lines ?see:row.see col.overflow width cell))
      (slots ?max_width t widths row.cells)
  in
  let height =
    List.fold_left
      (fun acc (_, _, lines) -> max acc (List.length lines))
      0 cells
  in
  let style col = Style.(style + col.style) in
  let render = if edged t then edged_line else open_line in
  List.init height (fun line_idx ->
      render t style
        (List.map
           (fun (col, width, lines) ->
             (col, width, List.nth_opt lines line_idx))
           cells))

let render_lines ?max_width t =
  let t =
    match max_width with None -> t | Some width -> fit_columns (max 0 width) t
  in
  if List.length t.columns = 0 then []
  else
    let chars = Border.chars t.border in
    let widths = calc_col_widths ?max_width t in
    let rule l m r h = Option.to_list (horizontal t widths l m r h) in
    let row style r = row_lines ?max_width t widths style r in
    let headers = List.map (fun col -> col.header) t.columns in
    let has_headers = List.exists (fun h -> Span.to_string h <> "") headers in
    let rows = List.rev t.rows in
    let body =
      List.concat
        (List.mapi
           (fun i r ->
             (if i > 0 && t.row_separators then
                rule chars.left_cross chars.cross chars.right_cross chars.top
              else [])
             @ row Style.none r)
           rows)
    in
    rule chars.top_left chars.top_cross chars.top_right chars.top
    @ (if has_headers then
         row t.header_style { cells = headers; see = None }
         @
         if rows <> [] then
           rule chars.left_cross chars.cross chars.right_cross chars.top
         else []
       else [])
    @ body
    @ rule chars.bottom_left chars.bottom_cross chars.bottom_right chars.bottom

let pp_with ?max_width ppf t =
  let styled = Fmt.style_renderer ppf = `Ansi_tty in
  let p = Paint.v ppf in
  let border = Border.style t.border in
  List.iter
    (fun line ->
      List.iter
        (function
          | Edge s -> Paint.ink p border s
          | Gap s -> Paint.text p s
          | Cell (style, span) ->
              Paint.text p
                (if styled then
                   Span.to_string ~styled (Span.with_style style span)
                 else Span.to_string span))
        line;
      Paint.newline p)
    (render_lines ?max_width t);
  Paint.close p

let lines ?width t =
  let border = Border.style t.border in
  List.map
    (fun line ->
      Span.concat
        (List.map
           (function
             | Edge s -> Span.styled border s
             | Gap s -> Span.text s
             | Cell (style, span) -> Span.with_style style span)
           line))
    (render_lines ?max_width:width t)

let pp ppf t =
  let max_width = Format.pp_get_margin ppf () in
  pp_with ~max_width ppf t

let to_string ?styled ?width t =
  Render.to_string (pp_with ?max_width:width) ?styled t
