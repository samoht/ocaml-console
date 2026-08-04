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
  char_width : Uchar.t -> int;
}

let check_geometry fn padding width =
  if padding < 0 then invalid_arg (fn ^ ": negative padding");
  match width with
  | Some width when width < 2 + (2 * padding) ->
      invalid_arg (fn ^ ": width too small for padding")
  | _ -> ()

let create ~fn ?theme ?border ?title ?subtitle ?(padding = 1) ?width
    ?(char_width = Width.default_char_width) lines =
  check_geometry fn padding width;
  let border = Widget.border ?theme ?border ~default:Border.rounded () in
  {
    border;
    animated = Widget.animated theme;
    title;
    subtitle;
    padding;
    width;
    lines;
    char_width;
  }

let v ?theme ?border ?title ?subtitle ?padding ?width ?char_width content =
  create ~fn:"Console.Panel.v" ?theme ?border ?title ?subtitle ?padding ?width
    ?char_width (Span.split_lines content)

let lines ?theme ?border ?title ?subtitle ?(padding = 1) ?width
    ?(char_width = Width.default_char_width) lines =
  create ~fn:"Console.Panel.lines" ?theme ?border ?title ?subtitle ~padding
    ?width ~char_width lines

let render_horizontal ppf border_style count char =
  for _ = 1 to count do
    Style.styled border_style Fmt.string ppf char
  done

let content_width panel =
  match panel.width with
  | Some total_w -> total_w - 2 - (panel.padding * 2)
  | None ->
      let max_line =
        List.fold_left
          (fun acc line ->
            max acc (Span.width ~char_width:panel.char_width line))
          0 panel.lines
      in
      let title_w =
        match panel.title with
        | Some span -> Span.width ~char_width:panel.char_width span
        | None -> 0
      in
      let subtitle_w =
        match panel.subtitle with
        | Some span -> Span.width ~char_width:panel.char_width span
        | None -> 0
      in
      max max_line (max title_w subtitle_w)

let render_span ppf ~char_width ~width span =
  let span = Span.sanitize ~keep_newlines:false span in
  let text =
    match Fmt.style_renderer ppf with
    | `Ansi_tty -> Span.to_ansi_string span
    | `None -> Span.to_string span
  in
  Width.truncate ~char_width width text

(* Render a horizontal border row (top or bottom) with optional embedded
   centered text. Used by both [render_top_border] (title) and
   [render_bottom_border] (subtitle). *)
let render_horiz_border ppf border_style ~char_width ~left_char ~mid_char
    ~right_char inner_width text =
  Style.styled border_style Fmt.string ppf left_char;
  (match text with
  | None -> render_horizontal ppf border_style inner_width mid_char
  | Some span when inner_width >= 3 ->
      let text = render_span ppf ~char_width ~width:(inner_width - 2) span in
      let w = Width.string_width ~char_width text in
      let left_border = (inner_width - w - 2) / 2 in
      let right_border = inner_width - w - 2 - left_border in
      render_horizontal ppf border_style left_border mid_char;
      Fmt.char ppf ' ';
      Fmt.string ppf text;
      Fmt.char ppf ' ';
      render_horizontal ppf border_style right_border mid_char
  | Some _ -> render_horizontal ppf border_style inner_width mid_char);
  Style.styled border_style Fmt.string ppf right_char

let render_top_border ppf panel border_style chars inner_width =
  render_horiz_border ppf border_style ~char_width:panel.char_width
    ~left_char:chars.Border.top_left ~mid_char:chars.Border.top
    ~right_char:chars.Border.top_right inner_width panel.title;
  Render.newline ppf

let render_content_lines ppf panel border_style chars cw pad =
  List.iter
    (fun line ->
      Style.styled border_style Fmt.string ppf chars.Border.left;
      Fmt.string ppf pad;
      let line = render_span ppf ~char_width:panel.char_width ~width:cw line in
      Fmt.string ppf line;
      let spaces = cw - Width.string_width ~char_width:panel.char_width line in
      if spaces > 0 then Fmt.string ppf (String.make spaces ' ');
      Fmt.string ppf pad;
      Style.styled border_style Fmt.string ppf chars.Border.right;
      Render.newline ppf)
    panel.lines

let render_bottom_border ppf panel border_style chars inner_width =
  render_horiz_border ppf border_style ~char_width:panel.char_width
    ~left_char:chars.Border.bottom_left ~mid_char:chars.Border.bottom
    ~right_char:chars.Border.bottom_right inner_width panel.subtitle

let pp ppf panel =
  let chars = Border.chars panel.border in
  let border_style = Border.style panel.border in
  let cw = content_width panel in
  let inner_width = cw + (panel.padding * 2) in
  let pad = String.make panel.padding ' ' in
  render_top_border ppf panel border_style chars inner_width;
  render_content_lines ppf panel border_style chars cw pad;
  render_bottom_border ppf panel border_style chars inner_width

let to_string panel = Render.to_string pp panel
let to_ansi_string panel = Render.to_string ~style_renderer:`Ansi_tty pp panel

(* The rain frames the content lines (each padded to the inner width) with a
   Matrix border. The title and subtitle are not drawn in this mode. *)
let rain panel =
  Anim.v (fun ~elapsed ->
      let cw = content_width panel in
      let pad = String.make panel.padding ' ' in
      let line l =
        let l =
          Span.sanitize ~keep_newlines:false l
          |> Span.to_ansi_string
          |> Width.truncate ~char_width:panel.char_width cw
        in
        pad ^ Width.pad_right ~char_width:panel.char_width cw l ^ pad
      in
      Rain.frame ~char_width:panel.char_width
        (List.map line panel.lines)
        ~elapsed)

(* A still frame, unless the theme asked for an animated border. *)
let anim panel =
  if panel.animated then rain panel else Anim.const (to_ansi_string panel)
