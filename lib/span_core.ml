(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Each atom's text is non-empty, valid UTF-8 and holds no control character
   but the line feed: the constructors make it so, and every operation below
   keeps it. *)
type atom = { style : Style.t; link : string option; text : string }
type t = atom list

let clean = Console_text.sanitize ~keep_newlines:true

(* Every linked atom is built here, so no link escapes the check: a target
   is printable ASCII without a space, which cannot end the OSC 8 sequence it
   is written in. *)
let atom ?link style text =
  Option.iter
    (fun uri ->
      if uri = "" then invalid_arg "Console.Span.link: empty URI";
      if String.exists (fun ch -> ch <= ' ' || ch >= '\x7f') uri then
        invalid_arg "Console.Span.link: unsafe URI character")
    link;
  match clean text with "" -> [] | text -> [ { style; link; text } ]

let empty = []
let text s = atom Style.none s
let styled style s = atom style s
let link ?(style = Style.none) ~uri s = atom ~link:uri style s
let ( ++ ) = ( @ )

let concat ?(sep = empty) = function
  | [] -> empty
  | x :: xs -> List.concat (x :: List.concat_map (fun y -> [ sep; y ]) xs)

let with_style base t =
  List.map (fun a -> { a with style = Style.(base + a.style) }) t

(* {1 Lines} *)

let lines t =
  if not (List.exists (fun a -> String.contains a.text '\n') t) then [ t ]
  else
    let rec go lines line = function
      | [] -> List.rev (List.rev line :: lines)
      | a :: rest -> (
          match String.index_opt a.text '\n' with
          | None -> go lines (a :: line) rest
          | Some i ->
              let before = String.sub a.text 0 i in
              let after =
                String.sub a.text (i + 1) (String.length a.text - i - 1)
              in
              let line =
                if before = "" then line else { a with text = before } :: line
              in
              let rest =
                if after = "" then rest else { a with text = after } :: rest
              in
              go (List.rev line :: lines) [] rest)
    in
    go [] [] t

let unlines = function
  | [] -> empty
  | l :: ls -> List.concat (l :: List.map (fun l -> text "\n" @ l) ls)

(* {1 Layout}

   A line is laid out as the grapheme clusters a terminal draws, each with its
   width. Clusters are found over the line's whole text, so a mark that
   combines with the letter before it joins that letter even across a change
   of style; a cluster takes the style and link of the atom it starts in. A
   layout keeps offsets into the joined text rather than a value per
   cluster. *)

type layout = {
  joined : string;  (** The line's text. *)
  parts : atom array;
  part_starts : int array;  (** Where each atom starts in [text]. *)
  n : int;  (** The number of clusters. *)
  starts : int array;  (** Where each cluster starts; [[||]] when ASCII. *)
  widths : int array;  (** Each cluster's width; [[||]] when ASCII. *)
}

let ascii line = List.for_all (fun a -> Width.is_ascii a.text) line

let layout line =
  let parts = Array.of_list line in
  let part_starts = Array.make (Array.length parts) 0 in
  let total =
    Array.fold_left
      (fun (i, off) a ->
        part_starts.(i) <- off;
        (i + 1, off + String.length a.text))
      (0, 0) parts
    |> snd
  in
  let text = String.concat "" (List.map (fun a -> a.text) line) in
  if ascii line then
    {
      joined = text;
      parts;
      part_starts;
      n = total;
      starts = [||];
      widths = [||];
    }
  else begin
    let starts = ref [] and widths = ref [] and n = ref 0 in
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        starts := offset :: !starts;
        widths :=
          Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:1 text
            ~pos:offset ~len
          :: !widths;
        incr n)
      text;
    let starts = Array.of_list (List.rev !starts) in
    let widths = Array.of_list (List.rev !widths) in
    { joined = text; parts; part_starts; n = !n; starts; widths }
  end

let start l i =
  if l.starts = [||] then i
  else if i >= l.n then String.length l.joined
  else l.starts.(i)

let cell_width l i = if l.widths = [||] then 1 else l.widths.(i)
let is_space l i = l.joined.[start l i] = ' ' && start l (i + 1) = start l i + 1

(* The atom cluster [i] starts in. *)
let owner l i =
  let off = start l i in
  let k = ref (Array.length l.parts - 1) in
  while !k > 0 && l.part_starts.(!k) > off do
    decr k
  done;
  l.parts.(!k)

(* The atoms of clusters [i] to [j - 1]. *)
let sub l i j =
  if i >= j then []
  else
    let a = start l i and b = start l j in
    let acc = ref [] in
    Array.iteri
      (fun k p ->
        let ps = l.part_starts.(k) in
        let pe = ps + String.length p.text in
        let s = max a ps and e = min b pe in
        if s < e then
          acc := { p with text = String.sub l.joined s (e - s) } :: !acc)
      l.parts;
    List.rev !acc

let line_width = function
  | [ a ] -> Width.string_width a.text
  | line ->
      if ascii line then
        List.fold_left (fun n a -> n + String.length a.text) 0 line
      else
        Width.string_width (String.concat "" (List.map (fun a -> a.text) line))

let width t = List.fold_left (fun n l -> max n (line_width l)) 0 (lines t)

(* {1 Truncating} *)

let ellipsis = "\xe2\x80\xa6"

let truncate_line w line =
  if line_width line <= w then line
  else
    let l = layout line in
    let rec take i room =
      if i < l.n && cell_width l i <= room then
        take (i + 1) (room - cell_width l i)
      else i
    in
    let k = take 0 (w - 1) in
    let style = if k < l.n then (owner l k).style else Style.none in
    sub l 0 k @ [ { style; link = None; text = ellipsis } ]

let truncate w t =
  if w <= 0 then empty else unlines (List.map (truncate_line w) (lines t))

(* {1 Wrapping} *)

(* A line being broken into lines of at most [w] cells, those after the
   first [hang] cells in: the lines done, newest first, and the ranges of
   clusters of the line under way, newest first. *)
type filler = {
  l : layout;
  w : int;
  hang : int;
  mutable out : t list;
  mutable cur : (int * int) list;
  mutable cur_w : int;
  mutable first : bool;
}

let room f = if f.first then f.w else f.w - f.hang

let range_width l i j =
  let n = ref 0 in
  for k = i to j - 1 do
    n := !n + cell_width l k
  done;
  !n

let add f i j =
  (match f.cur with
  | (a, b) :: rest when b = i -> f.cur <- (a, j) :: rest
  | _ -> f.cur <- (i, j) :: f.cur);
  f.cur_w <- f.cur_w + range_width f.l i j

(* The spaces a break stands on are not drawn. *)
let rec trim l = function
  | (a, b) :: rest when b > a && is_space l (b - 1) ->
      trim l (if b - 1 > a then (a, b - 1) :: rest else rest)
  | r -> r

let finish f =
  let body =
    List.concat_map (fun (a, b) -> sub f.l a b) (List.rev (trim f.l f.cur))
  in
  let body =
    if f.first || f.hang = 0 then body
    else
      { style = Style.none; link = None; text = String.make f.hang ' ' } :: body
  in
  f.out <- body :: f.out;
  f.cur <- [];
  f.cur_w <- 0;
  f.first <- false

let has_text f =
  List.exists
    (fun (a, b) ->
      let r = ref false in
      for k = a to b - 1 do
        if not (is_space f.l k) then r := true
      done;
      !r)
    f.cur

(* A word wider than the room goes on where the room ends; a cluster wider
   than a whole line cannot be drawn and is left out. *)
let split f i j =
  for k = i to j - 1 do
    let cw = cell_width f.l k in
    if cw <= room f then begin
      if f.cur_w + cw > room f then finish f;
      add f k (k + 1)
    end
  done

let rec word f i j =
  if f.cur_w + range_width f.l i j <= room f then add f i j
  else if has_text f then begin
    finish f;
    word f i j
  end
  else split f i j

let spaces f i j =
  if f.cur = [] && not f.first then ()
  else if f.cur_w + range_width f.l i j <= room f then add f i j
  else if f.cur <> [] then finish f

(* Words end at a run of spaces, or after a slash. *)
let rec fill f i =
  let l = f.l in
  if i < l.n then begin
    let j = ref i in
    if is_space l i then begin
      while !j < l.n && is_space l !j do
        incr j
      done;
      spaces f i !j
    end
    else begin
      let stop = ref false in
      while (not !stop) && !j < l.n && not (is_space l !j) do
        let slash = l.joined.[start l !j] = '/' in
        incr j;
        if slash then stop := true
      done;
      word f i !j
    end;
    fill f !j
  end

let wrap_line ~hang w line =
  let f =
    { l = layout line; w; hang; out = []; cur = []; cur_w = 0; first = true }
  in
  fill f 0;
  if f.cur <> [] || f.out = [] then finish f;
  List.rev f.out

let widest_word t =
  List.fold_left
    (fun acc line ->
      let l = layout line in
      let best = ref acc and run = ref 0 in
      for i = 0 to l.n - 1 do
        if is_space l i then run := 0
        else begin
          run := !run + cell_width l i;
          best := max !best !run
        end
      done;
      !best)
    0 (lines t)

(* The cells a line's continuation hangs under: its leading spaces, its first
   word and the spaces after it, when more text follows. *)
let hanging line =
  let l = layout line in
  let i = ref 0 and w = ref 0 in
  let skip p =
    while !i < l.n && p !i do
      w := !w + cell_width l !i;
      incr i
    done
  in
  skip (is_space l);
  skip (fun k -> not (is_space l k));
  skip (is_space l);
  if !i >= l.n then 0 else !w

let wrap_with ~hanging:h w t =
  if w <= 0 then invalid_arg "Console.Span.wrap: non-positive width";
  List.concat_map
    (fun line ->
      let hang = if h then max 0 (min (hanging line) (w / 2)) else 0 in
      wrap_line ~hang w line)
    (lines t)

let wrap w t = wrap_with ~hanging:false w t
let wrap_hanging w t = wrap_with ~hanging:true w t

(* {1 Rendering} *)

let osc8_open uri = "\027]8;;" ^ uri ^ "\027\\"
let osc8_close = "\027]8;;\027\\"

(* Neighbouring atoms of one style and link are written under one escape
   code. *)
let runs t =
  let rec go acc = function
    | a :: b :: rest
      when Style.equal a.style b.style
           && Option.equal String.equal a.link b.link ->
        go acc ({ a with text = a.text ^ b.text } :: rest)
    | a :: rest -> go (a :: acc) rest
    | [] -> List.rev acc
  in
  go [] t

(* Styles and links are closed before each line feed and opened again after
   it, so that no line leaves them on for what the terminal prints next. *)
let add_styled buf ~depth t =
  List.iter
    (fun { style; link; text } ->
      let sgr = Style.sgr depth style in
      List.iteri
        (fun i line ->
          if i > 0 then Buffer.add_char buf '\n';
          if line <> "" then begin
            Option.iter (fun uri -> Buffer.add_string buf (osc8_open uri)) link;
            Buffer.add_string buf sgr;
            Buffer.add_string buf line;
            if sgr <> "" then Buffer.add_string buf Sgr.reset;
            Option.iter (fun _ -> Buffer.add_string buf osc8_close) link
          end)
        (String.split_on_char '\n' text))
    (runs t)

let render ?depth t =
  let buf = Buffer.create 64 in
  (match depth with
  | None -> List.iter (fun a -> Buffer.add_string buf a.text) t
  | Some depth -> add_styled buf ~depth t);
  Buffer.contents buf

let to_string ?(styled = false) t =
  render ?depth:(if styled then Some `True_color else None) t

let pp ppf t =
  match Fmt.style_renderer ppf with
  | `None -> List.iter (fun a -> Format.pp_print_string ppf a.text) t
  | `Ansi_tty -> Format.pp_print_string ppf (render ~depth:(Color.depth ppf) t)

let pp_lines ppf lines =
  List.iteri
    (fun i line ->
      if i > 0 then Format.pp_print_char ppf '\n';
      pp ppf line)
    lines

let string_of_lines ?(styled = false) lines =
  String.concat "\n" (List.map (to_string ~styled) lines)
