(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type atom = { style : Style.t; link : string option; text : string }
type t = atom list

let atom ?link style text = { style; link; text }
let text str = [ atom Style.none str ]
let styled style str = [ atom style str ]

let link ?(style = Style.none) ~uri str =
  if uri = "" then invalid_arg "Console.Span.link: empty URI";
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if code <= 0x20 || code = 0x7f then
        invalid_arg "Console.Span.link: unsafe URI character")
    uri;
  [ atom ~link:uri style str ]

let empty = []
let space = text " "
let newline = text "\n"
let concat spans = List.concat spans
let ( ++ ) a b = a @ b

let sanitize ?(keep_newlines = true) t =
  let clean = Render.sanitize ~keep_newlines in
  List.map (fun atom -> { atom with text = clean atom.text }) t

let concat_map ?(sep = empty) f xs =
  match xs with
  | [] -> empty
  | [ x ] -> f x
  | x :: xs -> List.fold_left (fun acc x -> acc ++ sep ++ f x) (f x) xs

let split_lines t =
  let lines = ref [] in
  let line = ref [] in
  let push_line () =
    lines := List.rev !line :: !lines;
    line := []
  in
  let push_text style link text =
    if text <> "" then line := atom ?link style text :: !line
  in
  let push_atom { style; link; text } =
    let parts = String.split_on_char '\n' text in
    let rec loop = function
      | [] -> ()
      | [ part ] -> push_text style link part
      | part :: rest ->
          push_text style link part;
          push_line ();
          loop rest
    in
    loop parts
  in
  List.iter push_atom t;
  push_line ();
  List.rev !lines

let osc8_open uri = "\027]8;;" ^ uri ^ "\027\\"
let osc8_close = "\027]8;;\027\\"

let pp_atom base ppf { style; link; text } =
  let ansi = Style.to_ansi Style.(base + style) in
  Option.iter (fun uri -> Fmt.string ppf (osc8_open uri)) link;
  if ansi <> "" then Fmt.string ppf ansi;
  Fmt.string ppf text;
  if ansi <> "" then Fmt.string ppf Style.reset;
  Option.iter (fun _ -> Fmt.string ppf osc8_close) link

(** Render without ANSI styling. Used whenever the caller has disabled colour on
    the target formatter. *)
let pp_plain ppf t = List.iter (fun { text; _ } -> Fmt.string ppf text) t

(** Whether to emit ANSI on [ppf]. Defers to [Fmt.style_renderer] — set by
    {!Fmt_tty.setup_std_outputs} (which ocaml-observe calls based on its
    [--color] / [NO_COLOR] cmdliner terms). *)
let should_style ppf =
  match Fmt.style_renderer ppf with `Ansi_tty -> true | `None -> false

let pp_with_style style ppf t =
  if should_style ppf then List.iter (pp_atom style ppf) t else pp_plain ppf t

let pp = pp_with_style Style.none
let to_string t = Render.to_string pp t

let to_ansi_string ?(style = Style.none) t =
  Render.to_string ~style_renderer:`Ansi_tty (pp_with_style style) t

let width ?(char_width = Width.default_char_width) t =
  List.fold_left
    (fun acc { text; _ } -> acc + Width.string_width ~char_width text)
    0 t

let truncate ?(char_width = Width.default_char_width) target t =
  if target <= 0 then empty
  else
    let rec loop remaining acc = function
      | [] -> List.rev acc
      | atom :: rest ->
          let atom_width = Width.string_width ~char_width atom.text in
          if atom_width <= remaining then
            loop (remaining - atom_width) (atom :: acc) rest
          else
            let text = Width.truncate ~char_width remaining atom.text in
            let acc = if text = "" then acc else { atom with text } :: acc in
            List.rev acc
    in
    loop target [] t

let tokens t =
  let result = ref [] and current = ref [] in
  let add atom text =
    if text <> "" then current := { atom with text } :: !current
  in
  let flush () =
    if !current <> [] then begin
      result := List.rev !current :: !result;
      current := []
    end
  in
  List.iter
    (fun atom ->
      let start = ref 0 in
      for i = 0 to String.length atom.text - 1 do
        match atom.text.[i] with
        | ' ' ->
            add atom (String.sub atom.text !start (i - !start));
            flush ();
            add atom " ";
            start := i + 1
        | '/' ->
            add atom (String.sub atom.text !start (i - !start + 1));
            flush ();
            start := i + 1
        | _ -> ()
      done;
      add atom (String.sub atom.text !start (String.length atom.text - !start)))
    t;
  flush ();
  List.rev !result

let trim_leading_space = function
  | ({ text; _ } as atom) :: rest when text <> "" && text.[0] = ' ' ->
      let text = String.sub text 1 (String.length text - 1) in
      if text = "" then rest else { atom with text } :: rest
  | token -> token

let wrap ?(char_width = Width.default_char_width) target t =
  if target <= 0 then invalid_arg "Console.Span.wrap: non-positive width";
  let finish line acc = if line = empty then acc else line :: acc in
  let rec loop acc line line_width = function
    | [] -> List.rev (finish line acc)
    | token :: rest ->
        let token_width = width ~char_width token in
        if line_width + token_width <= target then
          loop acc (line ++ token) (line_width + token_width) rest
        else if line <> empty then
          loop (line :: acc) empty 0 (trim_leading_space token :: rest)
        else
          let shown =
            if token_width <= target then token
            else if target = 1 then truncate ~char_width 1 token
            else truncate ~char_width (target - 1) token ++ text "…"
          in
          loop (shown :: acc) empty 0 rest
  in
  match tokens t with [] -> [ empty ] | tokens -> loop [] empty 0 tokens

let anim t = Anim.const (to_ansi_string t)
