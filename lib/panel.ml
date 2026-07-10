(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  border : Border.t;
  animated : bool;
  title : Span.t option;
  subtitle : Span.t option;
  padding : int;
  width : int option;
  lines : Span.t list;
}

(* The border comes from [border] if given, else [theme]'s border, else the
   rounded default. *)
let resolve_border ?theme ?border () =
  match border with
  | Some b -> b
  | None -> (
      match theme with Some t -> t.Theme.border | None -> Border.rounded)

(* Whether the theme asks for an animated (rain) border. *)
let resolve_animated theme =
  match theme with Some t -> t.Theme.animated_border | None -> false

let v ?theme ?border ?title ?subtitle ?(padding = 1) ?width content =
  let border = resolve_border ?theme ?border () in
  let content_str = Span.to_plain_string content in
  let line_strs = String.split_on_char '\n' content_str in
  let lines = List.map Span.text line_strs in
  {
    border;
    animated = resolve_animated theme;
    title;
    subtitle;
    padding;
    width;
    lines;
  }

let lines ?theme ?border ?title ?subtitle ?(padding = 1) ?width lines =
  let border = resolve_border ?theme ?border () in
  {
    border;
    animated = resolve_animated theme;
    title;
    subtitle;
    padding;
    width;
    lines;
  }

let render_horizontal ppf border_style count char =
  for _ = 1 to count do
    Border.render_char ppf border_style char
  done

let content_width panel =
  match panel.width with
  | Some total_w -> total_w - 2 - (panel.padding * 2)
  | None ->
      let max_line =
        List.fold_left (fun acc line -> max acc (Span.width line)) 0 panel.lines
      in
      let title_w =
        match panel.title with Some span -> Span.width span | None -> 0
      in
      let subtitle_w =
        match panel.subtitle with Some span -> Span.width span | None -> 0
      in
      max max_line (max title_w subtitle_w)

(* Render a horizontal border row (top or bottom) with optional embedded
   centered text. Used by both [render_top_border] (title) and
   [render_bottom_border] (subtitle). *)
let render_horiz_border ppf border_style ~left_char ~mid_char ~right_char
    inner_width text =
  Border.render_char ppf border_style left_char;
  (match text with
  | None -> render_horizontal ppf border_style inner_width mid_char
  | Some span ->
      let w = Span.width span in
      let left_border = (inner_width - w - 2) / 2 in
      let right_border = inner_width - w - 2 - left_border in
      render_horizontal ppf border_style left_border mid_char;
      Fmt.char ppf ' ';
      Span.pp ppf span;
      Fmt.char ppf ' ';
      render_horizontal ppf border_style right_border mid_char);
  Border.render_char ppf border_style right_char

let render_top_border ppf panel border_style chars inner_width =
  render_horiz_border ppf border_style ~left_char:chars.Border.top_left
    ~mid_char:chars.Border.top ~right_char:chars.Border.top_right inner_width
    panel.title;
  Fmt.cut ppf ()

let render_content_lines ppf panel border_style chars cw pad =
  List.iter
    (fun line ->
      Border.render_char ppf border_style chars.Border.left;
      Fmt.string ppf pad;
      Span.pp ppf line;
      let spaces = cw - Span.width line in
      if spaces > 0 then Fmt.string ppf (String.make spaces ' ');
      Fmt.string ppf pad;
      Border.render_char ppf border_style chars.Border.right;
      Fmt.cut ppf ())
    panel.lines

let render_bottom_border ppf panel border_style chars inner_width =
  render_horiz_border ppf border_style ~left_char:chars.Border.bottom_left
    ~mid_char:chars.Border.bottom ~right_char:chars.Border.bottom_right
    inner_width panel.subtitle

let pp ppf panel =
  let chars = panel.border.chars in
  let border_style = panel.border.style in
  let cw = content_width panel in
  let inner_width = cw + (panel.padding * 2) in
  let pad = String.make panel.padding ' ' in
  render_top_border ppf panel border_style chars inner_width;
  render_content_lines ppf panel border_style chars cw pad;
  render_bottom_border ppf panel border_style chars inner_width

let to_string panel =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf panel;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The rain frames the content lines (each padded to the inner width) with a
   Matrix border. The title and subtitle are not drawn in this mode. *)
let rain panel =
  Anim.v (fun ~elapsed ->
      let cw = content_width panel in
      let pad = String.make panel.padding ' ' in
      let line l = pad ^ Width.pad_right cw (Span.to_string l) ^ pad in
      Rain.frame (List.map line panel.lines) ~elapsed)

(* A still frame, unless the theme asked for an animated border. *)
let anim panel =
  if panel.animated then rain panel else Anim.const (to_string panel)
