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

let atom ?link style text =
  match clean text with "" -> [] | text -> [ { style; link; text } ]

let empty = []
let text s = atom Style.none s
let styled style s = atom style s

let link ?(style = Style.none) ~uri s =
  if uri = "" then invalid_arg "Console.Span.link: empty URI";
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if code <= 0x20 || code >= 0x7f then
        invalid_arg "Console.Span.link: unsafe URI character")
    uri;
  atom ~link:uri style s

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

let is_ascii_text s =
  String.for_all (fun c -> Char.code c >= 0x20 && Char.code c < 0x7f) s

let ascii line = List.for_all (fun a -> is_ascii_text a.text) line

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

let measure = Glyph.String.measure ~width_method:`Unicode ~tab_width:1

let line_width = function
  | [] -> 0
  | [ a ] ->
      if is_ascii_text a.text then String.length a.text else measure a.text
  | line ->
      if ascii line then
        List.fold_left (fun n a -> n + String.length a.text) 0 line
      else measure (String.concat "" (List.map (fun a -> a.text) line))

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

(* The line broken into lines of at most [w] cells, those after the first
   [hang] cells in. A line is kept as the ranges of clusters it shows. *)
let wrap_line ~hang w line =
  let l = layout line in
  let out = ref [] and cur = ref [] and cur_w = ref 0 and first = ref true in
  let room () = if !first then w else w - hang in
  let add i j wd =
    (match !cur with
    | (a, b) :: rest when b = i -> cur := (a, j) :: rest
    | _ -> cur := (i, j) :: !cur);
    cur_w := !cur_w + wd
  in
  let finish () =
    (* The spaces a break stands on are not drawn. *)
    let rec trim = function
      | (a, b) :: rest when b > a && is_space l (b - 1) ->
          trim (if b - 1 > a then (a, b - 1) :: rest else rest)
      | r -> r
    in
    let body =
      List.concat_map (fun (a, b) -> sub l a b) (List.rev (trim !cur))
    in
    let body =
      if !first || hang = 0 then body
      else
        { style = Style.none; link = None; text = String.make hang ' ' } :: body
    in
    out := body :: !out;
    cur := [];
    cur_w := 0;
    first := false
  in
  let range_width i j =
    let n = ref 0 in
    for k = i to j - 1 do
      n := !n + cell_width l k
    done;
    !n
  in
  let has_text () =
    List.exists
      (fun (a, b) ->
        let r = ref false in
        for k = a to b - 1 do
          if not (is_space l k) then r := true
        done;
        !r)
      !cur
  in
  (* A word wider than the room goes on where the room ends; a cluster wider
     than a whole line cannot be drawn and is left out. *)
  let split i j =
    for k = i to j - 1 do
      let cw = cell_width l k in
      if cw <= room () then begin
        if !cur_w + cw > room () then finish ();
        add k (k + 1) cw
      end
    done
  in
  let rec word i j =
    let ww = range_width i j in
    if !cur_w + ww <= room () then add i j ww
    else if has_text () then begin
      finish ();
      word i j
    end
    else split i j
  in
  let spaces i j =
    if !cur = [] && not !first then ()
    else
      let sw = range_width i j in
      if !cur_w + sw <= room () then add i j sw
      else if !cur <> [] then finish ()
  in
  (* Words end at a run of spaces, or after a slash. *)
  let rec go i =
    if i < l.n then
      if is_space l i then begin
        let j = ref i in
        while !j < l.n && is_space l !j do
          incr j
        done;
        spaces i !j;
        go !j
      end
      else begin
        let j = ref i and stop = ref false in
        while (not !stop) && !j < l.n && not (is_space l !j) do
          let slash = l.joined.[start l !j] = '/' in
          incr j;
          if slash then stop := true
        done;
        word i !j;
        go !j
      end
  in
  go 0;
  if !cur <> [] || !out = [] then finish ();
  List.rev !out

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

let add_styled buf ~depth t =
  List.iter
    (fun { style; link; text } ->
      let sgr = Style.to_ansi ~depth style in
      Option.iter (fun uri -> Buffer.add_string buf (osc8_open uri)) link;
      Buffer.add_string buf sgr;
      Buffer.add_string buf text;
      if sgr <> "" then Buffer.add_string buf Style.reset;
      Option.iter (fun _ -> Buffer.add_string buf osc8_close) link)
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

let lines_to_string ?(styled = false) lines =
  String.concat "\n" (List.map (to_string ~styled) lines)
