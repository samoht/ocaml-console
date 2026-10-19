(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type cell = string Inked.t

let cell ?(style = Style.none) glyph =
  if
    Width.string_width glyph <> 1
    || Render.sanitize ~keep_newlines:false glyph <> glyph
    || String.contains glyph '\027'
  then invalid_arg "Console.Canvas.cell: not one terminal cell";
  Inked.v ~style glyph

let replacement = "\xef\xbf\xbd"

let cells ?(style = Style.none) s =
  let s = Render.sanitize ~keep_newlines:false s in
  List.rev
    (List.fold_left
       (fun acc (g, w) ->
         match (w, acc) with
         | 0, c :: rest ->
             Inked.v ~style:(Inked.style c) (Inked.get c ^ g) :: rest
         | 0, [] -> acc
         | 1, _ -> Inked.v ~style g :: acc
         | _ -> Inked.v ~style replacement :: acc)
       [] (Width.atoms s))

let glyph = Inked.get
let style = Inked.style

type t = { width : int; cells : cell array array }

let v ~width ~height f =
  if width < 0 || height < 0 then
    invalid_arg "Console.Canvas.v: negative dimension";
  {
    width;
    cells =
      Array.init height (fun row ->
          Array.init width (fun column -> f ~row ~column));
  }

let of_rows rows =
  let width = match rows with [] -> 0 | row :: _ -> List.length row in
  if List.exists (fun row -> List.length row <> width) rows then
    invalid_arg "Console.Canvas.of_rows: rows of different lengths";
  { width; cells = Array.of_list (List.map Array.of_list rows) }

let width c = c.width
let height c = Array.length c.cells

let get c ~row ~column =
  if row < 0 || row >= height c || column < 0 || column >= c.width then
    invalid_arg "Console.Canvas.get: position outside the canvas";
  c.cells.(row).(column)

let map f c =
  {
    c with
    cells =
      Array.mapi
        (fun row cells -> Array.mapi (fun column -> f ~row ~column) cells)
        c.cells;
  }

(* A row as runs of cells that draw the same sequence. *)
let row_span row cells =
  let runs =
    Array.fold_left
      (fun (column, acc) cell ->
        let style = Style.at ~row ~column (Inked.style cell) in
        let sgr = Style.to_ansi style in
        let acc =
          match acc with
          | (text, style', sgr') :: rest when String.equal sgr sgr' ->
              (text ^ Inked.get cell, style', sgr') :: rest
          | _ -> (Inked.get cell, style, sgr) :: acc
        in
        (column + 1, acc))
      (0, []) cells
    |> snd
  in
  Span.concat
    (List.rev_map (fun (text, style, _) -> Span.styled style text) runs)

let rows c = Array.to_list (Array.mapi row_span c.cells)

let pp ppf c =
  let margin = Format.pp_get_margin ppf () in
  List.iteri
    (fun i row ->
      if i > 0 then Render.newline ppf;
      Span.pp ppf (Span.truncate margin row))
    (rows c)
