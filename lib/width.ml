(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let fold_tokens = Console_text.fold_tokens

let string_width str =
  (* Styling controls do not break a terminal grapheme. Strip them before
     segmentation so a combining mark or ZWJ sequence spanning an ANSI reset
     has exactly the same width here as it does in the terminal. Measuring each
     visible run separately over-counts standalone combining marks. *)
  let visible = Buffer.create (String.length str) in
  fold_tokens
    (fun () kind pos len ->
      match kind with
      | `Text -> Buffer.add_substring visible str pos len
      | `Escape -> ())
    () str;
  Glyph.String.measure ~width_method:`Unicode ~tab_width:2
    (Buffer.contents visible)

let is_link e = String.starts_with ~prefix:"\027]8;" e

let closes_link e =
  String.equal e "\027]8;;\027\\" || String.equal e "\027]8;;\007"

(* Whatever [rendered] leaves open, closed: a style the last SGR left on, and
   a hyperlink whose closing OSC 8 is missing. *)
let closing rendered =
  let style, link =
    fold_tokens
      (fun ((style, link) as state) kind pos len ->
        match kind with
        | `Text -> state
        | `Escape -> (
            let e = String.sub rendered pos len in
            match Console_text.sgr e with
            | Some `Set -> (true, link)
            | Some `Reset -> (false, link)
            | None when is_link e -> (style, not (closes_link e))
            | None -> state))
      (false, false) rendered
  in
  (if style then "\027[0m" else "") ^ if link then "\027]8;;\027\\" else ""

(* [str] with each malformed UTF-8 sequence U+FFFD. *)
let normalize_utf8 str =
  let n = String.length str in
  let buffer = Buffer.create n in
  let rec go i =
    if i < n then begin
      let d = String.get_utf_8_uchar str i in
      Buffer.add_utf_8_uchar buffer
        (if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
         else Uchar.rep);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.contents buffer

(* [str] as the pieces a terminal draws it in: an escape sequence, which takes
   no column and is never cut, or one grapheme cluster with the width it
   occupies. *)
let atoms str =
  let str = normalize_utf8 str in
  let pieces =
    fold_tokens
      (fun pieces kind pos len ->
        match kind with
        | `Escape -> (String.sub str pos len, 0) :: pieces
        | `Text ->
            let run = String.sub str pos len in
            let pieces = ref pieces in
            Glyph.String.iter_graphemes
              (fun ~offset ~len ->
                pieces :=
                  ( String.sub run offset len,
                    Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:2
                      run ~pos:offset ~len )
                  :: !pieces)
              run;
            !pieces)
      [] str
  in
  List.rev pieces

(* The pieces of [atoms], in the order they are given, that fit in [target]
   columns. Reversed atoms therefore yield the value's last [target] columns. *)
let take target atoms =
  let rec go width acc = function
    | (piece, w) :: rest when width + w <= target ->
        go (width + w) (piece :: acc) rest
    | _ -> List.rev acc
  in
  go 0 [] atoms

let truncate target_width str =
  if target_width <= 0 then ""
  else
    let atoms = atoms str in
    let taken = take target_width atoms in
    let kept = String.concat "" taken in
    if List.compare_lengths taken atoms = 0 then kept else kept ^ closing kept

let ellipsis = "..."
let is_space (piece, _) = String.equal piece " "

let is_escape (piece, width) =
  width = 0 && String.length piece > 0 && piece.[0] = '\027'

(* Where a row may lose its end: a space outside any bracket, so a group such
   as "(300s bound)" goes as one word and never leaves "(300s" behind. A
   bracket that never closes groups nothing, and every space is a break. *)
let breaks atoms =
  let depth = ref 0 in
  let grouped =
    List.map
      (fun ((piece, _) as atom) ->
        (match piece with
        | "(" | "[" | "{" -> incr depth
        | ")" | "]" | "}" -> depth := max 0 (!depth - 1)
        | _ -> ());
        (atom, !depth = 0 && is_space atom))
      atoms
  in
  if !depth = 0 then grouped
  else List.map (fun atom -> (atom, is_space atom)) atoms

(* A run of words that lost its tail ends on a word, not on the separator the
   next word needed. *)
let rec trim_end = function
  | (piece, _) :: rest
    when String.equal piece " " || String.equal piece ","
         || String.equal piece ";" ->
      trim_end rest
  | atoms -> atoms

(* The words that only lead into the words after them: a row that lost those
   words does not end on one ("building in", "waiting for"). *)
let leading_words =
  [
    "a";
    "an";
    "and";
    "as";
    "at";
    "by";
    "for";
    "from";
    "in";
    "into";
    "of";
    "on";
    "or";
    "the";
    "to";
    "via";
    "with";
  ]

(* [kept_rev], reversed atoms, without the separators and the leading words at
   its end, nor a colon that introduced what went, nor a middle dot that joined
   it to the next clause. *)
let rec settle kept_rev =
  match trim_end kept_rev with
  | (":", _) :: rest -> settle rest
  | kept_rev -> (
      let rec last_word word = function
        | atom :: rest when not (is_space atom) -> last_word (atom :: word) rest
        | rest -> (word, rest)
      in
      let word, rest = last_word [] kept_rev in
      let text =
        String.lowercase_ascii (String.concat "" (List.map fst word))
      in
      match rest with
      | _ :: _ when String.equal text "\xc2\xb7" || List.mem text leading_words
        ->
          settle rest
      | _ -> kept_rev)

let shorten target_width str =
  if string_width str <= target_width then str
  else if target_width <= 0 then ""
  else
    let rec walk kept_rev width best = function
      | [] -> best
      | (((_, piece_width) as atom), break) :: rest ->
          let best =
            if break && width <= target_width then kept_rev else best
          in
          if width > target_width then best
          else walk (atom :: kept_rev) (width + piece_width) best rest
    in
    let kept_rev = walk [] 0 [] (breaks (atoms str)) in
    match List.rev (settle kept_rev) with
    | [] -> ""
    | kept ->
        let head = String.concat "" (List.map fst kept) in
        head ^ closing head

(* The items of [str], each with the separator before it ([""] for the
   first): its pieces between the [", "] and the [" \xc2\xb7 "] outside any
   bracket. A bracket that never closes groups nothing. *)
let items str =
  let len = String.length str in
  let dot = " \xc2\xb7 " in
  let at i sep =
    i + String.length sep <= len
    && String.equal (String.sub str i (String.length sep)) sep
  in
  let split ~grouped =
    let depth = ref 0 in
    let rec go acc sep from i =
      if i >= len then List.rev ((sep, String.sub str from (len - from)) :: acc)
      else
        match str.[i] with
        | ('(' | '[' | '{') when grouped ->
            incr depth;
            go acc sep from (i + 1)
        | (')' | ']' | '}') when grouped ->
            depth := max 0 (!depth - 1);
            go acc sep from (i + 1)
        | ',' when !depth = 0 && at i ", " ->
            go
              ((sep, String.sub str from (i - from)) :: acc)
              ", " (i + 2) (i + 2)
        | ' ' when !depth = 0 && at i dot ->
            let n = String.length dot in
            go ((sep, String.sub str from (i - from)) :: acc) dot (i + n) (i + n)
        | _ -> go acc sep from (i + 1)
    in
    let items = go [] "" 0 0 in
    (items, !depth = 0)
  in
  match split ~grouped:true with
  | items, true -> items
  | _, false -> fst (split ~grouped:false)

let more n = if n = 0 then "" else " and " ^ string_of_int n ^ " more"

let elide target_width str =
  let width = string_width in
  if width str <= target_width then str
  else
    let alone () = if width ellipsis <= target_width then ellipsis else "" in
    match items str with
    | [] -> alone ()
    | (_, first) :: rest -> (
        let total = 1 + List.length rest in
        let fits kept k =
          let c = kept ^ more (total - k) in
          if width c <= target_width then Some c else None
        in
        let rec longest kept k best = function
          | [] -> best
          | (sep, item) :: rest -> (
              let kept = kept ^ sep ^ item in
              match fits kept (k + 1) with
              | Some c -> longest kept (k + 1) c rest
              | None -> best)
        in
        let cut () =
          let tail = ellipsis ^ more (total - 1) in
          match shorten (target_width - width tail) first with
          | "" -> alone ()
          | kept -> kept ^ tail
        in
        if total = 1 then cut ()
        else
          match fits first 1 with
          | None -> cut ()
          | Some c -> longest first 1 c rest)

(* The escapes still in force after [piece]: a reset ends every style, a
   hyperlink's close ends the hyperlink, and anything else is added. *)
let in_force active piece =
  let sgr p = Option.is_some (Console_text.sgr p) in
  if Console_text.sgr piece = Some `Reset then
    List.filter (fun p -> not (sgr p)) active
  else if closes_link piece then List.filter (fun p -> not (is_link p)) active
  else active @ [ piece ]

(* [atoms] as runs of words and runs of spaces, each with its width. *)
let runs atoms =
  let flush current acc =
    match current with
    | [] -> acc
    | _ :: _ as run ->
        let run = List.rev run in
        (run, List.fold_left (fun n (_, w) -> n + w) 0 run) :: acc
  in
  let rec go current kind acc = function
    | [] -> List.rev (flush current acc)
    | atom :: rest ->
        let space = is_space atom in
        if (not (is_escape atom)) && space <> kind && current <> [] then
          go [ atom ] space (flush current acc) rest
        else if current = [] then go [ atom ] space acc rest
        else go (atom :: current) kind acc rest
  in
  go [] false [] atoms

(* The line [fold] is filling: its atoms, newest first, and its width; the
   lines it has finished, newest first; and the escapes in force, at the end
   of the line and where it began. *)
type folding = {
  mutable lines : string list;
  mutable line : (string * int) list;
  mutable width : int;
  mutable active : string list;
  mutable opened : string list;
}

let add f ((piece, piece_width) as atom) =
  f.line <- atom :: f.line;
  f.width <- f.width + piece_width;
  if is_escape atom then f.active <- in_force f.active piece

let finish f =
  let text = String.concat "" (List.rev_map fst (trim_end f.line)) in
  let text = String.concat "" f.opened ^ text in
  f.lines <- (text ^ closing (String.concat "" f.active)) :: f.lines;
  f.line <- [];
  f.width <- 0;
  f.opened <- f.active

let blank f = List.for_all is_escape f.line
let run_width run = List.fold_left (fun n (_, w) -> n + w) 0 run

(* The atoms of a word wider than a line that fit on the line under way,
   which starts blank; a glyph wider than the whole line goes on alone. What
   is left of the word is returned. *)
let rec fill f target_width = function
  | [] -> []
  | ((_, w) as atom) :: rest ->
      if f.width + w <= target_width || (blank f && w > target_width) then begin
        add f atom;
        fill f target_width rest
      end
      else atom :: rest

let rec place f ~split target_width run =
  let fits = f.width + run_width run <= target_width in
  match run with
  | [] -> ()
  | first :: _ when is_space first ->
      (* A line after a break starts at its word, not at the spaces the break
         stood on; the first line keeps its indentation. *)
      if blank f && f.lines <> [] then ()
      else if fits then List.iter (add f) run
      else if not (blank f) then finish f
  | _ when fits -> List.iter (add f) run
  | _ when not (blank f) ->
      finish f;
      place f ~split target_width run
  | _ when not split ->
      List.iter (add f) run;
      finish f
  | _ -> (
      (* The word alone is wider than a line: it goes on, whole, at the start
         of the next one. *)
      match fill f target_width run with
      | [] -> ()
      | rest ->
          finish f;
          place f ~split target_width rest)

let fold ?(split = false) target_width str =
  if target_width <= 0 || string_width str <= target_width then [ str ]
  else
    let f = { lines = []; line = []; width = 0; active = []; opened = [] } in
    List.iter
      (fun (run, _) -> place f ~split target_width run)
      (runs (atoms str));
    if (not (blank f)) || f.lines = [] then finish f;
    List.rev f.lines

let pad_right target_width str =
  let str_width = string_width str in
  if str_width >= target_width then str
  else str ^ String.make (target_width - str_width) ' '

let pad_left target_width str =
  let str_width = string_width str in
  if str_width >= target_width then str
  else String.make (target_width - str_width) ' ' ^ str

let rec wrap_words ~effective acc line len = function
  | [] -> if line = "" then acc else line :: acc
  | word :: rest ->
      let wlen = string_width word in
      let space = if line = "" then 0 else 1 in
      if len + space + wlen <= effective then
        let line = if line = "" then word else line ^ " " ^ word in
        wrap_words ~effective acc line (len + space + wlen) rest
      else if line = "" then
        (* Word longer than width; accept it on its own line *)
        wrap_words ~effective (word :: acc) "" 0 rest
      else wrap_words ~effective (line :: acc) word wlen rest

(* A backquoted run is one word: a row never ends inside a command the reader
   copies back. A backquote that never closes groups nothing. *)
let backquoted words =
  let odd word =
    String.fold_left (fun n c -> if c = '`' then n + 1 else n) 0 word mod 2 = 1
  in
  let rec close run = function
    | [] -> None
    | word :: rest when odd word -> Some (List.rev (word :: run), rest)
    | word :: rest -> close (word :: run) rest
  in
  let rec go acc = function
    | [] -> List.rev acc
    | word :: rest when odd word -> (
        match close [ word ] rest with
        | Some (run, rest) -> go (String.concat " " run :: acc) rest
        | None -> go (word :: acc) rest)
    | word :: rest -> go (word :: acc) rest
  in
  go [] words

let wrap ?(indent = 0) width text =
  let effective = width - indent in
  if effective <= 0 then text
  else
    let prefix = String.make indent ' ' in
    let normalized =
      text |> String.split_on_char '\n' |> List.map String.trim
      |> String.concat " "
    in
    let words = backquoted (String.split_on_char ' ' normalized) in
    let lines = List.rev (wrap_words ~effective [] "" 0 words) in
    String.concat "\n" (List.map (fun l -> prefix ^ l) lines)
