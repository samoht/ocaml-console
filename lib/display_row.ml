(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Ansi
open Display_state

let percent cur total =
  if total <= 0 then 0
  else if cur >= total then 100
  else int_of_float (100. *. (float cur /. float total))

let percent64 cur total =
  if total <= 0L then 0
  else if cur >= total then 100
  else int_of_float (100. *. (Int64.to_float cur /. Int64.to_float total))

let pct_of = function
  | Spin -> None
  | Count (cur, total) -> Some (percent cur total)
  | Bytes (cur, total) -> Some (percent64 cur total)

(* A row and the header use one marker vocabulary, so the same state never has
   two glyphs. *)
let render_spinner t ~finished ~color ~time row =
  let theme = t.theme in
  if not finished then
    styled t (color_code t color)
      (Spinner.frame (Theme.spinner theme) ~elapsed:time)
  else
    match row.routcome with
    | Some `Failed -> styled t (bold ^ color_code t (Theme.fail theme)) "[x]"
    | Some `Cancelled -> styled t (bold ^ color_code t (Theme.warn theme)) "[~]"
    | Some `Succeeded | None ->
        styled t
          (bold ^ color_code t (Theme.ok theme))
          (Theme.done_marker theme)

(* No bar is drawn before work advances, since an empty track reads as an
   input field; once it has, the bar shows at least one eighth of a cell. *)
let render_bar t row ~finished ~color ~width =
  let draw frac =
    let frac = Float.max frac (1. /. float_of_int (max 1 (width * 8))) in
    Span.to_string ~styled:t.styling
      (Bar.v ~brackets:true
         ~style:(Style.fg (Color.downsample t.depth color))
         ~width frac)
  in
  if finished then ""
  else
    match row.metric with
    | Spin | Count (0, _) | Bytes (0L, _) -> ""
    | Count (cur, total) -> draw (float_of_int cur /. float_of_int (max 1 total))
    | Bytes (cur, total) ->
        draw (Int64.to_float cur /. Int64.to_float (Int64.max 1L total))

let render_percent t row ~finished =
  if finished then ""
  else
    match pct_of row.metric with
    | Some p -> Fmt.kstr (dimmed t) "%3d%%" p
    | None -> ""

(* The padding is outside the styling, so an empty cell carries no escapes. *)
let dim_cell t width text =
  if text = "" then String.make (max 0 width) ' '
  else Width.pad_left width (dimmed t text)

let render_count t row = dim_cell t t.count_cell (count_text row.metric)

let render_rate t row ~finished =
  if finished then ""
  else
    match row.metric with
    | Bytes _ when row.rate > 1. ->
        dimmed t (Display_text.bytes (Int64.of_float row.rate) ^ "/s")
    | _ -> ""

let span_line t span = Span.to_string ~styled:t.styling span

let render_piece t row ~finished ~color ~time (piece : Display_line.piece) =
  match piece with
  | Spinner -> render_spinner t ~finished ~color ~time row
  | Label | Spacer -> ""
  | Bar n -> render_bar t row ~finished ~color ~width:n
  | Percent -> render_percent t row ~finished
  | Count -> render_count t row
  | Rate -> render_rate t row ~finished
  | Elapsed ->
      dim_cell t Display_text.elapsed_cell (Display_text.span (elapsed t row))
  | Text s -> s
  | Accent s -> styled t (bold ^ color_code t color) s

(* The label is the flexible content and a single [Spacer] absorbs the
   remaining slack, so later pieces sit at the right margin. *)
let live t row =
  let finished = row.rfinish <> None in
  let { Display_line.pieces; color } = row.style in
  let time = t.now () -. t.dstart in
  let rendered =
    List.map (fun p -> (p, render_piece t row ~finished ~color ~time p)) pieces
  in
  let indent_s = guide_prefix t row in
  let has_label =
    List.exists (fun (p, _) -> Display_line.is_label p) rendered
  in
  let has_spacer =
    List.exists (fun (p, _) -> Display_line.is_spacer p) rendered
  in
  let fixed_w =
    Width.string_width indent_s
    + List.fold_left
        (fun acc (p, s) ->
          match p with
          | Display_line.Label | Spacer -> acc
          | _ -> acc + Width.string_width s)
        0 rendered
  in
  let label_text =
    if finished then Option.value row.rmessage ~default:row.rlabel
    else row.rlabel
  in
  (* Reserve one column for the spacer so the pieces flanking it never touch. *)
  let reserve = if has_spacer then 1 else 0 in
  let width = t.width in
  let avail = max 0 (width - fixed_w - reserve) in
  let label_shown =
    if not has_label then ""
    else if Width.string_width label_text <= avail then label_text
    else Width.shorten avail label_text
  in
  let slack = max reserve (width - fixed_w - Width.string_width label_shown) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf indent_s;
  List.iter
    (fun ((p : Display_line.piece), s) ->
      match p with
      | Label -> Buffer.add_string buf (styled t bold label_shown)
      | Spacer -> Buffer.add_string buf (String.make slack ' ')
      | _ -> Buffer.add_string buf s)
    rendered;
  (* Fixed pieces alone may be wider than the terminal, and a wrapped row
     breaks the one-row-per-line redraw. *)
  Width.truncate width (Buffer.contents buf)

let committed_parts t row =
  let label = Option.value row.rmessage ~default:row.rlabel in
  let marker =
    (* A row that reaches history with no outcome is one the scrollback
       collapsed while it was still running, which is not an outcome a theme
       can name. *)
    match row.routcome with
    | Some outcome -> Theme.committed_marker t.theme outcome
    | None -> ".."
  in
  (indent row, marker, label, Display_text.span (elapsed t row))

let committed_marker_style t row =
  match row.routcome with
  | Some `Succeeded -> bold ^ color_code t (Theme.ok t.theme)
  | Some `Failed -> bold ^ color_code t (Theme.fail t.theme)
  | Some `Cancelled -> bold ^ color_code t (Theme.warn t.theme)
  | None -> dim

let appended t row =
  let indent, marker, label, elapsed = committed_parts t row in
  let metric =
    match count_text row.metric with "" -> "" | text -> "  " ^ text
  in
  let metadata = metric ^ "  " ^ elapsed in
  Fmt.str "%s%s %s%s" indent
    (styled t (committed_marker_style t row) marker)
    label (dimmed t metadata)

let committed t row =
  let marker_style = committed_marker_style t row in
  let indent, marker, label, elapsed = committed_parts t row in
  let width = t.width in
  let marker = styled t marker_style marker in
  let fixed_width = Width.string_width indent + Width.string_width marker + 1 in
  (* The elapsed time reaches the right edge in a cell of its own and the
     count sits in the block's cell before it, as on the live row. A terminal
     too narrow for both keeps the timer. *)
  let timer = Width.pad_left Display_text.elapsed_cell elapsed in
  let metadata =
    if t.count_cell = 0 then timer
    else Width.pad_left t.count_cell (count_text row.metric) ^ "  " ^ timer
  in
  let metadata =
    if fixed_width + 2 + Width.string_width metadata <= width then metadata
    else timer
  in
  let metadata_width = Width.string_width metadata in
  let label_width = max 0 (width - fixed_width - metadata_width - 2) in
  (* History has no redraw to protect, so a label that does not fit folds
     onto the lines below it rather than losing words; a word wider than the
     room stands whole and the terminal wraps it. *)
  let first, rest =
    match Width.fold label_width label with
    | first :: rest when Width.string_width first <= label_width -> (first, rest)
    | lines -> ("", lines)
  in
  let left = String.concat "" [ indent; marker; " "; first ] in
  let gap = width - Width.string_width left - metadata_width in
  let head =
    if gap > 0 then left ^ String.make gap ' ' ^ dimmed t metadata
    else Width.truncate width (left ^ dimmed t metadata)
  in
  let under = String.make fixed_width ' ' in
  String.concat "\n" (head :: List.map (fun line -> under ^ line) rest)

(* A gap of a second or more since the previous line stands out. *)
let log_gap_threshold = 1.0

let gap_suffix gap =
  if gap >= log_gap_threshold then "  +" ^ Display_text.span gap else ""

let level_marker theme = function
  | `Info -> Theme.note_marker theme
  | `Warning -> Theme.warn_marker theme
  | `Error -> Theme.error_marker theme

let level_style t = function
  | `Info -> dim
  | `Warning -> bold ^ color_code t (Theme.warn t.theme)
  | `Error -> bold ^ color_code t (Theme.fail t.theme)

let styled_level t level marker = styled t (level_style t level) marker

let styled_log t level marker message suffix =
  String.concat ""
    [ styled_level t level marker; " "; message; dimmed t suffix ]

let tail t row =
  let prefix = continuation t row in
  let lines =
    List.concat_map
      (fun log ->
        let marker = level_marker t.theme log.llevel in
        let suffix = gap_suffix log.lgap in
        (* The message gives way when the room runs out, words from its end;
           one whose first word does not fit folds under the marker, whole. *)
        let room =
          t.width - Width.string_width prefix - Width.string_width marker - 1
          - Width.string_width suffix
        in
        let lines =
          match Width.shorten room log.lmessage with
          | "" when room > 0 -> Width.fold ~split:true room log.lmessage
          | "" -> []
          | shown -> [ shown ]
        in
        let under = String.make (Width.string_width marker) ' ' in
        List.mapi
          (fun i line ->
            let last = i = List.length lines - 1 in
            Width.truncate t.width
              (String.concat ""
                 [
                   prefix;
                   (if i = 0 then
                      styled_log t log.llevel marker line
                        (if last then suffix else "")
                    else
                      under ^ " " ^ line ^ if last then dimmed t suffix else "");
                 ]))
          lines)
      row.logs
  in
  let excess = List.length lines - row.log_lines in
  if excess > 0 then List.filteri (fun i _ -> i >= excess) lines else lines

let committed_log t r entry =
  let marker = level_marker t.theme entry.llevel in
  let suffix = gap_suffix entry.lgap in
  continuation t r ^ styled_log t entry.llevel marker entry.lmessage suffix

let appended_log t r entry =
  Fmt.str "%s  %s %s%s" (indent r)
    (styled_level t entry.llevel (level_marker t.theme entry.llevel))
    entry.lmessage
    (dimmed t (gap_suffix entry.lgap))

let event t level message =
  match level with
  | `Info -> message
  | (`Warning | `Error) as level ->
      styled_level t level (level_marker t.theme level) ^ " " ^ message
