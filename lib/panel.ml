(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  border : Border.t;
  title : Span.t option;
  subtitle : Span.t option;
  padding : int;
  width : int option;
  lines : Span.t list;
}

let check_geometry fn padding width =
  if padding < 0 then invalid_arg (fn ^ ": negative padding");
  match width with
  | Some width when width < 2 + (2 * padding) ->
      invalid_arg (fn ^ ": width too small for padding")
  | _ -> ()

let build ~fn ?theme ?border ?title ?subtitle ?(padding = 1) ?width lines =
  check_geometry fn padding width;
  let border = Widget.border ?theme ?border ~default:Border.rounded () in
  { border; title; subtitle; padding; width; lines }

let v ?theme ?border ?title ?subtitle ?padding ?width content =
  build ~fn:"Console.Panel.v" ?theme ?border ?title ?subtitle ?padding ?width
    (Span.split_lines content)

let render_horizontal p border_style count char =
  for _ = 1 to count do
    Paint.ink p border_style char
  done

let natural_width panel =
  let max_line =
    List.fold_left (fun acc line -> max acc (Span.width line)) 0 panel.lines
  in
  let label = function Some span -> Span.width span | None -> 0 in
  max max_line (max (label panel.title) (label panel.subtitle))

(* The cells a content line has: the fixed width's room, else the content's
   own, capped by what [margin] leaves beside the border and padding. *)
let content_width ~margin panel =
  let frame = 2 + (panel.padding * 2) in
  let room =
    match panel.width with
    | Some total_w -> total_w - frame
    | None -> natural_width panel
  in
  max 1 (min room (margin - frame))

(* The lines as drawn in [cw] cells: in a box sized to its content, one wider
   than that wraps at its words, hanging under the text after its label; a
   fixed width truncates instead. *)
let fitted_lines panel cw =
  List.concat_map
    (fun line ->
      let line = Span.sanitize ~keep_newlines:false line in
      if panel.width <> None || Span.width line <= cw then [ line ]
      else Span.wrap_hanging cw line)
    panel.lines

let render_span ppf ~width span =
  let span = Span.sanitize ~keep_newlines:false span in
  let text =
    match Fmt.style_renderer ppf with
    | `Ansi_tty -> Span.to_string ~styled:true span
    | `None -> Span.to_string span
  in
  Width.truncate width text

(* Render a horizontal border row (top or bottom) with optional embedded
   centered text. Used by both [render_top_border] (title) and
   [render_bottom_border] (subtitle). *)
let render_horiz_border ppf p border_style ~left_char ~mid_char ~right_char
    inner_width text =
  Paint.ink p border_style left_char;
  (match text with
  | None -> render_horizontal p border_style inner_width mid_char
  | Some span when inner_width >= 3 ->
      let text = render_span ppf ~width:(inner_width - 2) span in
      let w = Width.string_width text in
      let left_border = (inner_width - w - 2) / 2 in
      let right_border = inner_width - w - 2 - left_border in
      render_horizontal p border_style left_border mid_char;
      Paint.text p (" " ^ text ^ " ");
      render_horizontal p border_style right_border mid_char
  | Some _ -> render_horizontal p border_style inner_width mid_char);
  Paint.ink p border_style right_char

let render_top_border ppf p panel border_style chars inner_width =
  render_horiz_border ppf p border_style ~left_char:chars.Border.top_left
    ~mid_char:chars.Border.top ~right_char:chars.Border.top_right inner_width
    panel.title;
  Paint.newline p

let render_content_lines ppf p panel border_style chars cw pad =
  List.iter
    (fun line ->
      Paint.ink p border_style chars.Border.left;
      let line = render_span ppf ~width:cw line in
      let spaces = cw - Width.string_width line in
      Paint.text p
        (pad ^ line ^ (if spaces > 0 then String.make spaces ' ' else "") ^ pad);
      Paint.ink p border_style chars.Border.right;
      Paint.newline p)
    (fitted_lines panel cw)

let render_bottom_border ppf p panel border_style chars inner_width =
  render_horiz_border ppf p border_style ~left_char:chars.Border.bottom_left
    ~mid_char:chars.Border.bottom ~right_char:chars.Border.bottom_right
    inner_width panel.subtitle;
  Paint.close p

let pp ppf panel =
  let chars = Border.chars panel.border in
  let border_style = Border.style panel.border in
  let cw = content_width ~margin:(Format.pp_get_margin ppf ()) panel in
  let inner_width = cw + (panel.padding * 2) in
  let pad = String.make panel.padding ' ' in
  let p = Paint.v ppf in
  render_top_border ppf p panel border_style chars inner_width;
  render_content_lines ppf p panel border_style chars cw pad;
  render_bottom_border ppf p panel border_style chars inner_width

let to_string = Render.to_string pp
