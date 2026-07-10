(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type atom = { style : Style.t; text : string }
type t = atom list

let text str = [ { style = Style.none; text = str } ]
let styled style str = [ { style; text = str } ]
let empty = []
let space = text " "
let newline = text "\n"
let concat spans = List.concat spans
let ( ++ ) a b = a @ b

let concat_map ?(sep = empty) f xs =
  match xs with
  | [] -> empty
  | [ x ] -> f x
  | x :: xs -> List.fold_left (fun acc x -> acc ++ sep ++ f x) (f x) xs

let pp_atom ppf { style; text } =
  let ansi = Style.to_ansi style in
  if ansi = "" then Fmt.string ppf text
  else (
    Fmt.string ppf ansi;
    Fmt.string ppf text;
    Fmt.string ppf Style.reset)

(** Render without ANSI styling. Used whenever the caller has disabled colour on
    the target formatter. *)
let pp_plain ppf t = List.iter (fun { text; _ } -> Fmt.string ppf text) t

(** Whether to emit ANSI on [ppf]. Defers to [Fmt.style_renderer] — set by
    {!Fmt_tty.setup_std_outputs} (which ocaml-observe calls based on its
    [--color] / [NO_COLOR] cmdliner terms). *)
let should_style ppf =
  match Fmt.style_renderer ppf with `Ansi_tty -> true | `None -> false

let pp ppf t =
  if should_style ppf then List.iter (pp_atom ppf) t else pp_plain ppf t

let to_string (t : t) =
  (* The explicit "with ANSI" renderer: emit styling unconditionally rather than
     deferring to the fresh buffer's style renderer (which is never
     [`Ansi_tty]). Callers wanting plain text use {!to_plain_string}; callers
     wanting the target formatter's colour policy use {!pp}. The [t] annotation
     keeps the inferred source type [t] (not the unfolded [atom list]) so the
     [to_] name reads as the [Bytes.to_string] pattern. *)
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  List.iter (pp_atom ppf) t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_plain_string t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp_plain ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let width ?(char_width = Width.default_char_width) t =
  List.fold_left
    (fun acc { text; _ } -> acc + Width.string_width ~char_width text)
    0 t

let anim t = Anim.const (to_string t)
