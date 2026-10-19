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

(* {1 Cells}

   A line is laid out as the grapheme clusters a terminal draws, each with its
   width and the style and link of the atom it starts in. Clusters are found
   over the line's whole text, so a mark that combines with the letter before
   it joins that letter even across a change of style. *)

type cell = { g : string; w : int; s : Style.t; l : string option }

let is_ascii_text s =
  String.for_all (fun c -> Char.code c >= 0x20 && Char.code c < 0x7f) s

let cells line =
  if List.for_all (fun a -> is_ascii_text a.text) line then
    List.concat_map
      (fun a ->
        List.init (String.length a.text) (fun i ->
            { g = String.make 1 a.text.[i]; w = 1; s = a.style; l = a.link }))
      line
  else
    let joined = String.concat "" (List.map (fun a -> a.text) line) in
    let owners = Array.make (String.length joined) (Style.none, None) in
    ignore
      (List.fold_left
         (fun off a ->
           String.iteri
             (fun i _ -> owners.(off + i) <- (a.style, a.link))
             a.text;
           off + String.length a.text)
         0 line);
    let acc = ref [] in
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        let s, l = owners.(offset) in
        let w =
          Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:1 joined
            ~pos:offset ~len
        in
        acc := { g = String.sub joined offset len; w; s; l } :: !acc)
      joined;
    List.rev !acc

(* The atoms of [cells], neighbours of one style and link merged. *)
let of_cells cells =
  let flush acc = function
    | [] -> acc
    | c :: _ as run ->
        {
          style = c.s;
          link = c.l;
          text = String.concat "" (List.rev_map (fun c -> c.g) run);
        }
        :: acc
  in
  let rec go acc run = function
    | [] -> List.rev (flush acc run)
    | c :: rest -> (
        match run with
        | r :: _ when Style.equal r.s c.s && Option.equal String.equal r.l c.l
          ->
            go acc (c :: run) rest
        | _ -> go (flush acc run) [ c ] rest)
  in
  go [] [] cells

let cells_width = List.fold_left (fun n c -> n + c.w) 0

let line_width line =
  if List.for_all (fun a -> is_ascii_text a.text) line then
    List.fold_left (fun n a -> n + String.length a.text) 0 line
  else cells_width (cells line)

let width t = List.fold_left (fun n l -> max n (line_width l)) 0 (lines t)

(* {1 Truncating} *)

let ellipsis = "\xe2\x80\xa6"

let truncate_line w line =
  if line_width line <= w then line
  else
    let rec take room acc = function
      | c :: rest when c.w <= room -> take (room - c.w) (c :: acc) rest
      | rest ->
          let s = match rest with c :: _ -> c.s | [] -> Style.none in
          List.rev ({ g = ellipsis; w = 1; s; l = None } :: acc)
    in
    of_cells (take (w - 1) [] (cells line))

let truncate w t =
  if w <= 0 then empty else unlines (List.map (truncate_line w) (lines t))

(* {1 Wrapping} *)

let is_space c = c.g = " "

(* The line's cells as words and runs of spaces, a word also ending after a
   slash so that a path can break between its segments. *)
let tokens cells =
  let rec go acc word = function
    | [] -> List.rev (if word = [] then acc else `Word (List.rev word) :: acc)
    | c :: _ as l when is_space c ->
        let rec spaces run = function
          | c :: rest when is_space c -> spaces (c :: run) rest
          | rest -> (List.rev run, rest)
        in
        let run, rest = spaces [] l in
        let acc = if word = [] then acc else `Word (List.rev word) :: acc in
        go (`Space run :: acc) [] rest
    | c :: rest when c.g = "/" ->
        go (`Word (List.rev (c :: word)) :: acc) [] rest
    | c :: rest -> go acc (c :: word) rest
  in
  go [] [] cells

let wrap_line ~hang w line =
  let indent =
    List.init hang (fun _ -> { g = " "; w = 1; s = Style.none; l = None })
  in
  let lines = ref [] and cur = ref [] and cur_w = ref 0 and first = ref true in
  let room () = if !first then w else w - hang in
  let finish () =
    let rec trim = function c :: rest when is_space c -> trim rest | l -> l in
    let body = List.rev (trim !cur) in
    let body = if !first then body else indent @ body in
    lines := of_cells body :: !lines;
    cur := [];
    cur_w := 0;
    first := false
  in
  let add c =
    cur := c :: !cur;
    cur_w := !cur_w + c.w
  in
  let has_text () = List.exists (fun c -> not (is_space c)) !cur in
  (* A word wider than the room goes on where the room ends; a cluster wider
     than a whole line cannot be drawn and is left out. *)
  let split word =
    List.iter
      (fun c ->
        if c.w <= room () then begin
          if !cur_w + c.w > room () then finish ();
          add c
        end)
      word
  in
  let rec place = function
    | `Space run ->
        if !cur = [] && not !first then ()
        else if !cur_w + cells_width run <= room () then List.iter add run
        else if !cur <> [] then finish ()
    | `Word word ->
        if !cur_w + cells_width word <= room () then List.iter add word
        else if has_text () then begin
          finish ();
          place (`Word word)
        end
        else split word
  in
  List.iter place (tokens (cells line));
  if !cur <> [] || !lines = [] then finish ();
  List.rev !lines

(* The cells a line's continuation hangs under: its leading spaces, its first
   word and the spaces after it, when more text follows. *)
let hanging line =
  let rec go n seen_word = function
    | c :: rest when is_space c -> go (n + c.w) seen_word rest
    | _ :: _ as l when not seen_word ->
        let rec word n = function
          | c :: rest when not (is_space c) -> word (n + c.w) rest
          | rest -> (n, rest)
        in
        let n, rest = word n l in
        go n true rest
    | _ :: _ -> n
    | [] -> 0
  in
  go 0 false (cells line)

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
