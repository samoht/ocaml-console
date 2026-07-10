(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type overflow = [ `Truncate | `Wrap ]

type column = {
  header : Span.t;
  align : align;
  min_width : int option;
  max_width : int option;
  overflow : overflow;
  shrinkable : bool;
  style : Style.t;
}

type t = {
  border : Border.t;
  animated : bool;
  header_style : Style.t;
  columns : column list;
  rows : Span.t list list;
}

let column ?(align = `Left) ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  {
    header = Span.text header;
    align;
    min_width;
    max_width;
    overflow;
    shrinkable;
    style;
  }

let column' ?(align = `Left) ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  { header; align; min_width; max_width; overflow; shrinkable; style }

(* The border comes from [border] if given, else [theme]'s border, else the
   single-line default. *)
let resolve_border ?theme ?border () =
  match border with
  | Some b -> b
  | None -> (
      match theme with Some t -> t.Theme.border | None -> Border.single)

let v ?theme ?border ?(header_style = Style.bold) columns =
  {
    border = resolve_border ?theme ?border ();
    animated =
      (match theme with Some t -> t.Theme.animated_border | None -> false);
    header_style;
    columns;
    rows = [];
  }

let add_row cells t = { t with rows = t.rows @ [ cells ] }
let add_row_strings strings t = add_row (List.map Span.text strings) t

let of_rows ?theme ?border ?header_style columns rows =
  let t = v ?theme ?border ?header_style columns in
  List.fold_left (fun t row -> add_row row t) t rows

let of_string_rows ?theme ?border ?header_style columns rows =
  of_rows ?theme ?border ?header_style columns
    (List.map (List.map Span.text) rows)

(* Calculate column widths *)
let apply_header_widths ~char_width t widths =
  List.iteri
    (fun i col ->
      widths.(i) <- max widths.(i) (Span.width ~char_width col.header))
    t.columns

let apply_row_widths ~char_width t widths =
  let num_cols = Array.length widths in
  List.iter
    (fun row ->
      List.iteri
        (fun i cell ->
          if i < num_cols then
            widths.(i) <- max widths.(i) (Span.width ~char_width cell))
        row)
    t.rows

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

(* Shrink columns to fit [max_width] columns when given. With no width to fit --
   a pipe or file -- squeezing would wrap or clip cell text (e.g. a long event
   name to "sp…"), corrupting output meant to be read or machine-ingested, so
   the natural widths are kept. *)
let shrink_widths_to_terminal ?max_width t widths =
  let num_cols = Array.length widths in
  match max_width with
  | None -> widths
  | Some term_w ->
      let border_overhead = (num_cols * 3) + 1 in
      let total () = Array.fold_left ( + ) 0 widths + border_overhead in
      let cols = Array.of_list t.columns in
      let min_of i = match cols.(i).min_width with Some m -> m | None -> 1 in
      (* Only columns marked shrinkable and still above their minimum can give up
         width; the widest such column yields first. A column marked
         [~shrinkable:false] (e.g. a package-name column that must never be cut)
         is never chosen, so the table overflows rather than truncating it. *)
      let can_shrink i = cols.(i).shrinkable && widths.(i) > min_of i in
      let done_ = ref false in
      while total () > term_w && not !done_ do
        let max_i = ref (-1) in
        let max_w = ref 0 in
        Array.iteri
          (fun i w ->
            if can_shrink i && w > !max_w then (
              max_w := w;
              max_i := i))
          widths;
        if !max_i < 0 then done_ := true
        else
          let excess = total () - term_w in
          let shrink = min excess (!max_w - min_of !max_i) in
          widths.(!max_i) <- !max_w - shrink
      done;
      widths

let calc_col_widths ~char_width ?max_width t =
  let widths = Array.make (List.length t.columns) 0 in
  apply_header_widths ~char_width t widths;
  apply_row_widths ~char_width t widths;
  apply_width_constraints t widths;
  shrink_widths_to_terminal ?max_width t widths |> Array.to_list

(* Emit a border glyph with its style's ANSI on a terminal, plain otherwise. The
   [styled] flag is the context's [is_tty]: piped or captured output carries no
   escapes, matching how Display rows render. *)
let put_border ~styled ppf style ch =
  if styled then Border.render_char ppf style ch else Fmt.string ppf ch

let render_horizontal ~styled ppf t widths left_char mid_char right_char
    horiz_char =
  let chars = t.border.chars in
  let border_style = t.border.style in
  let put = put_border ~styled ppf border_style in
  if chars.top = "" then () (* No border *)
  else (
    put left_char;
    List.iteri
      (fun i col_width ->
        for _ = 1 to col_width + 2 do
          (* +2 for padding *)
          put horiz_char
        done;
        if i < List.length widths - 1 then put mid_char)
      widths;
    put right_char;
    Fmt.string ppf "\n")

let align_line ~char_width align width text =
  let text_width = Width.string_width ~char_width text in
  if text_width >= width then text
  else
    match align with
    | `Left -> Width.pad_right ~char_width width text
    | `Right -> Width.pad_left ~char_width width text
    | `Center -> Width.center ~char_width width text

let split_tokens text =
  (* Split on spaces and after '/' to allow path wrapping. Spaces are kept as a
     prefix on the following token so that concatenation in [wrap_text]
     preserves word spacing. *)
  let buf = Buffer.create 16 in
  let tokens = ref [] in
  String.iter
    (fun c ->
      match c with
      | ' ' ->
          if Buffer.length buf > 0 then (
            tokens := Buffer.contents buf :: !tokens;
            Buffer.clear buf);
          Buffer.add_char buf ' '
      | '/' ->
          Buffer.add_char buf '/';
          tokens := Buffer.contents buf :: !tokens;
          Buffer.clear buf
      | c -> Buffer.add_char buf c)
    text;
  if Buffer.length buf > 0 then tokens := Buffer.contents buf :: !tokens;
  List.rev !tokens

let truncate_token ~char_width width token tlen =
  if tlen <= width then token
  else if width <= 1 then Width.truncate ~char_width width token
  else Width.truncate ~char_width (width - 1) token ^ "…"

let trim_leading_space token =
  if token <> "" && token.[0] = ' ' then
    String.sub token 1 (String.length token - 1)
  else token

let add_wrap_token ~char_width ~width acc line len token =
  let tlen = Width.string_width ~char_width token in
  if len + tlen <= width then (acc, line ^ token, len + tlen)
  else if line = "" then
    (truncate_token ~char_width width token tlen :: acc, "", 0)
  else
    let token = trim_leading_space token in
    (line :: acc, token, Width.string_width ~char_width token)

let rec wrap_tokens ~char_width ~width acc line len = function
  | [] -> if line = "" then acc else line :: acc
  | token :: rest ->
      let acc, line, len =
        add_wrap_token ~char_width ~width acc line len token
      in
      wrap_tokens ~char_width ~width acc line len rest

let wrap_text ~char_width width text =
  if width <= 0 then [ text ]
  else
    let tokens = split_tokens text in
    match List.rev (wrap_tokens ~char_width ~width [] "" 0 tokens) with
    | [] -> [ "" ]
    | lines -> lines

(* Replace control bytes in cell text with a space while passing CSI escape
   sequences through untouched. A raw tab or backspace would move the terminal
   cursor and make the cell render wider or narrower than its measured width,
   breaking the column alignment; an escape would inject control sequences. *)
let sanitize_visible s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 and n = String.length s in
  while !i < n do
    if s.[!i] = '\027' && !i + 1 < n && s.[!i + 1] = '[' then begin
      Buffer.add_string b "\027[";
      i := !i + 2;
      while !i < n && not (s.[!i] >= '@' && s.[!i] <= '~') do
        Buffer.add_char b s.[!i];
        incr i
      done;
      if !i < n then (
        Buffer.add_char b s.[!i];
        incr i)
    end
    else begin
      let c = s.[!i] in
      Buffer.add_char b
        (if Char.code c < 0x20 || Char.code c = 0x7f then ' ' else c);
      incr i
    end
  done;
  Buffer.contents b

let process_cell ~char_width overflow align width span =
  let text = sanitize_visible (Span.to_string span) in
  let cell_width = Width.string_width ~char_width text in
  match overflow with
  | `Truncate ->
      if cell_width > width then
        (* Truncate with ellipsis *)
        let truncated =
          if width <= 1 then Width.truncate ~char_width width text
          else Width.truncate ~char_width (width - 1) text ^ "…"
        in
        [ truncated ]
      else [ align_line ~char_width align width text ]
  | `Wrap ->
      let lines = wrap_text ~char_width width text in
      List.map (align_line ~char_width align width) lines

let render_physical_row ~styled ppf t widths style line_cells =
  let chars = t.border.chars in
  let border_style = t.border.style in
  let num_cols = List.length widths in
  let put = put_border ~styled ppf border_style in

  if chars.left <> "" then put chars.left;
  List.iteri
    (fun i ((col, _col_width), line) ->
      Fmt.char ppf ' ';
      let cell_style = Style.(style + col.style) in
      let ansi = if styled then Style.to_ansi cell_style else "" in
      if ansi <> "" then Fmt.string ppf ansi;
      Fmt.string ppf line;
      if ansi <> "" then Fmt.string ppf Style.reset;
      Fmt.char ppf ' ';
      if i < num_cols - 1 && chars.left <> "" then put chars.right)
    (List.combine (List.combine t.columns widths) line_cells);
  if chars.right <> "" then put chars.right;
  Fmt.string ppf "\n"

let render_row ~char_width ~styled ppf t widths style cells =
  (* Index cells and widths by column once: the per-column [List.nth]/
     [List.length] this replaced made rendering a row quadratic in the column
     count. *)
  let cells_arr = Array.of_list cells in
  let ncells = Array.length cells_arr in
  let widths_arr = Array.of_list widths in
  (* Process each cell to get list of lines *)
  let cell_lines =
    List.mapi
      (fun i (col, col_width) ->
        let cell = if i < ncells then cells_arr.(i) else Span.empty in
        process_cell ~char_width col.overflow col.align col_width cell)
      (List.combine t.columns widths)
  in
  (* Find max number of lines *)
  let max_lines =
    List.fold_left (fun acc lines -> max acc (List.length lines)) 0 cell_lines
  in
  (* Render each physical row *)
  for line_idx = 0 to max_lines - 1 do
    let line_cells =
      List.mapi
        (fun i lines ->
          let col_width = widths_arr.(i) in
          if line_idx < List.length lines then List.nth lines line_idx
          else String.make col_width ' ')
        cell_lines
    in
    render_physical_row ~styled ppf t widths style line_cells
  done

let pp_with ~char_width ~styled ?max_width ppf t =
  if List.length t.columns = 0 then ()
  else
    let chars = t.border.chars in
    let widths = calc_col_widths ~char_width ?max_width t in

    (* Top border *)
    render_horizontal ~styled ppf t widths chars.top_left chars.top_cross
      chars.top_right chars.top;

    (* Header row (skip if all headers are empty) *)
    let headers = List.map (fun col -> col.header) t.columns in
    let has_headers =
      List.exists (fun h -> Span.to_plain_string h <> "") headers
    in
    if has_headers then (
      render_row ~char_width ~styled ppf t widths t.header_style headers;
      if List.length t.rows > 0 then
        render_horizontal ~styled ppf t widths chars.left_cross chars.cross
          chars.right_cross chars.top);

    (* Data rows *)
    List.iter
      (fun row -> render_row ~char_width ~styled ppf t widths Style.none row)
      t.rows;

    (* Bottom border *)
    render_horizontal ~styled ppf t widths chars.bottom_left chars.bottom_cross
      chars.bottom_right chars.bottom

(* Fit columns to the terminal width only when writing to one. Piped or captured
   output has no width to fit, so render at natural width and keep every cell
   whole. *)
let max_width_of_ctx ctx =
  if Display.is_tty ctx then Some (fst (Display.dimensions ctx)) else None

let pp ~ctx ppf t =
  pp_with ~char_width:(Display.char_width ctx) ~styled:(Display.is_tty ctx)
    ?max_width:(max_width_of_ctx ctx) ppf t

let to_string ~ctx t =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  pp ~ctx ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Frame the rendered table with a Matrix rain border. *)
let rain ~ctx t =
  Anim.v (fun ~elapsed ->
      let body =
        to_string ~ctx t |> String.split_on_char '\n'
        |> List.filter (fun l -> l <> "")
      in
      Rain.frame ~char_width:(Display.char_width ctx) body ~elapsed)

(* A still frame, unless the theme asked for an animated border. *)
let anim ~ctx t =
  if t.animated then rain ~ctx t else Anim.const (to_string ~ctx t)
