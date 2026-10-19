(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  ppf : Format.formatter;
  styling : bool;
  mutable row : int;
  mutable column : int;
  mutable open_ : (Style.t * string) option;
      (** The resolved style and SGR sequence of the run being written. *)
}

let v ppf =
  {
    ppf;
    styling = Fmt.style_renderer ppf = `Ansi_tty;
    row = 0;
    column = 0;
    open_ = None;
  }

let close p =
  match p.open_ with
  | None -> ()
  | Some _ ->
      Fmt.string p.ppf Style.reset;
      p.open_ <- None

let ink_cell p style sgr glyph width =
  (match p.open_ with
  | _ when sgr = "" || (glyph = " " && not (Style.shows_on_space style)) ->
      close p
  | Some (_, open_sgr) when open_sgr = sgr -> ()
  | _ ->
      close p;
      Fmt.string p.ppf sgr;
      p.open_ <- Some (style, sgr));
  Fmt.string p.ppf glyph;
  p.column <- p.column + width

let ink p style glyphs =
  if (not p.styling) || Style.is_none style then begin
    close p;
    Fmt.string p.ppf glyphs;
    p.column <- p.column + Width.string_width glyphs
  end
  else
    let sgr = Style.to_ansi style in
    List.iter
      (fun (g, width) -> ink_cell p style sgr g width)
      (Width.atoms glyphs)

let text p s =
  close p;
  Fmt.string p.ppf s;
  p.column <- p.column + Width.string_width s

let newline p =
  close p;
  Fmt.char p.ppf '\n';
  p.row <- p.row + 1;
  p.column <- 0
