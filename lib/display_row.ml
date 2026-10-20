(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let text = Span.text
let pad = Widget.pad

let pad_left n span =
  let w = Span.width span in
  if w >= n then span else Span.(pad (n - w) ++ span)

(* The display is drawn in the look of its formatter. *)
let look t = Widget.look t.ppf
let theme t = (look t).theme
let cut t w s = Widget.truncate (look t) w s
let muted t s = Span.styled (Widget.muted (look t)) s

(* Successive rows take the colours of the theme's palette in turn. *)
let color r =
  let palette = Theme.palette (theme r.disp) in
  List.nth palette (r.id mod List.length palette)

let mark_style t (m : Theme.mark) =
  let theme = theme t in
  match m with
  | `Succeeded -> Style.(bold + fg (Theme.ok theme))
  | `Failed | `Error -> Style.(bold + fg (Theme.fail theme))
  | `Cancelled | `Warning -> Style.(bold + fg (Theme.warn theme))
  | `Running -> Style.(bold + fg (Theme.accent theme))
  | `Info -> Widget.muted (look t)

let mark t m = Span.styled (mark_style t m) (Widget.mark (look t) m)
let outcome_mark (o : outcome) = (o :> Theme.mark)

(* {1 Guides} *)

let guide t = Widget.guide (look t) None

(* [lasts] are a row's flags, innermost first, ending with its root's, which
   draws nothing: a root row starts at the left edge. *)
let rec nested = function [] | [ _ ] -> [] | own :: up -> own :: nested up

let prefix t lasts =
  match nested lasts with [] -> "" | l -> "  " ^ Widget.branch (guide t) l

let continuation t lasts = "  " ^ Widget.rails (guide t) (nested lasts)
let indent r = String.make (2 * depth r) ' '

(* {1 Live rows} *)

let fraction = function
  | Spin -> None
  | Count (cur, total) -> Some (float cur /. float (max 1 total))
  | Bytes (cur, total) ->
      Some (Int64.to_float cur /. Int64.to_float (Int64.max 1L total))

(* In floats: [100 * cur] overflows an int long before [cur] does. *)
let percent m =
  Option.map
    (fun f -> if f >= 1. then 100 else int_of_float (100. *. f))
    (fraction m)

(* No bar is drawn before work advances, since an empty track reads as an
   input field; once it has, the bar shows the least it can. *)
let bar t r width =
  match fraction r.metric with
  | Some f when f > 0. ->
      (* A half cell, the least a bar shows, in UTF-8 and in ASCII alike. *)
      let f = Float.max f (1. /. float (max 1 (width * 2))) in
      Bar.span ~utf_8:(look t).utf_8 ~brackets:false
        ~style:(Style.fg (color r))
        ~track:(Widget.muted (look t))
        ~width f
  | _ -> Span.empty

let count_cell t r = pad_left t.count_cell (muted t (count_text r.metric))

let timer t d =
  pad_left Display_text.duration_cells (muted t (Display_text.duration d))

let piece t r ~time (p : Display_line.piece) =
  match p with
  | Spinner ->
      Span.styled (Style.fg (color r)) (Widget.spinner (look t) ~elapsed:time)
  | Label | Spacer -> Span.empty
  | Bar n -> bar t r n
  | Percent -> (
      match percent r.metric with
      | Some p -> Fmt.kstr (muted t) "%3d%%" p
      | None -> Span.empty)
  | Count -> count_cell t r
  | Rate -> (
      match r.metric with
      | Bytes _ when r.rate > 1. ->
          muted t (Display_text.bytes (Int64.of_float r.rate) ^ "/s")
      | _ -> Span.empty)
  | Elapsed -> timer t (elapsed r)
  | Text s -> text s
  | Accent s -> Span.styled Style.(bold + fg (color r)) s

(* The label takes the room the other pieces leave and is cut to it; one
   spacer takes the slack, so the pieces after it sit at the right edge. *)
let live t ~lasts r =
  let time = t.now () -. t.start in
  let pieces = List.map (fun p -> (p, piece t r ~time p)) t.layout in
  let prefix = prefix t lasts in
  let has p = List.exists (fun (q, _) -> q = p) pieces in
  let fixed =
    Span.width (Span.text prefix)
    + List.fold_left (fun n (_, s) -> n + Span.width s) 0 pieces
  in
  let reserve = if has Display_line.Spacer then 1 else 0 in
  let label =
    if has Display_line.Label then
      cut t (t.width - fixed - reserve) (Span.styled Style.bold r.label)
    else Span.empty
  in
  let slack = max reserve (t.width - fixed - Span.width label) in
  let line =
    Span.concat
      (Span.styled (Guide.style (guide t)) prefix
      :: List.map
           (fun ((p : Display_line.piece), s) ->
             match p with Label -> label | Spacer -> pad slack | _ -> s)
           pieces)
  in
  cut t t.width line

let gap_suffix t gap =
  if gap >= 1. then muted t ("  +" ^ Display_text.duration gap) else Span.empty

let tail t ~lasts r =
  let prefix = continuation t lasts in
  Queue.fold
    (fun acc log ->
      let line =
        Span.(
          styled (Guide.style (guide t)) prefix
          ++ mark t (log.level :> Theme.mark)
          ++ text " " ++ text log.message ++ gap_suffix t log.gap)
      in
      cut t t.width line :: acc)
    [] r.logs
  |> List.rev

(* {1 Permanent lines} *)

let label_of r = Option.value r.message ~default:r.label

let mark_of t r =
  mark t (outcome_mark (Option.value r.outcome ~default:`Cancelled))

(* On a terminal: the mark and the label, then the count and the timer
   against the right edge, in the cells every row uses, so the columns line
   up whatever the labels. A label that does not fit folds onto the lines
   below it. *)
let committed t r =
  let lead = Span.(text (indent r) ++ mark_of t r ++ text " ") in
  let lead_w = Span.width lead in
  let timer = timer t (elapsed r) in
  let meta =
    if t.count_cell = 0 then timer
    else Span.(count_cell t r ++ text "  " ++ timer)
  in
  let meta = if lead_w + 2 + Span.width meta <= t.width then meta else timer in
  let room = t.width - lead_w - Span.width meta - 2 in
  let label = text (label_of r) in
  let first, rest =
    if room <= 0 then (Span.empty, [ label ])
    else
      match Span.wrap room label with
      | first :: rest -> (first, rest)
      | [] -> (Span.empty, [])
  in
  let head = Span.(lead ++ first) in
  let gap = t.width - Span.width head - Span.width meta in
  let head = cut t t.width Span.(head ++ pad gap ++ meta) in
  let under = pad lead_w in
  head
  :: List.concat_map
       (fun l ->
         List.map
           (fun l -> cut t t.width Span.(under ++ l))
           (Span.wrap (max 1 (t.width - lead_w)) l))
       rest

(* Without a terminal: the same, compact, on one line. *)
let appended r =
  let t = r.disp in
  let count = match count_text r.metric with "" -> "" | c -> "  " ^ c in
  Span.(
    text (indent r)
    ++ mark_of t r ++ text " "
    ++ text (label_of r)
    ++ muted t (count ^ "  " ^ Display_text.duration (elapsed r)))

let committed_log r log =
  let t = r.disp in
  cut t t.width
    Span.(
      text (indent r ^ "  ")
      ++ mark t (log.level :> Theme.mark)
      ++ text " " ++ text log.message ++ gap_suffix t log.gap)

(* Without a terminal, lines of tasks that run at once are interleaved, so
   each says which task it is from. *)
let appended_log r log =
  Span.(
    text (indent r ^ "  ")
    ++ mark r.disp (log.level :> Theme.mark)
    ++ text " "
    ++ muted r.disp (r.label ^ ": ")
    ++ text log.message ++ gap_suffix r.disp log.gap)

let message t (level : level) msg =
  match level with
  | `Info -> text msg
  | (`Warning | `Error) as l ->
      Span.(mark t (l :> Theme.mark) ++ text " " ++ text msg)

(* {1 The header and the verdict} *)

let state t : Theme.mark =
  if (not t.closed) && (t.started = 0 || t.finished < t.started) then `Running
  else if
    t.failed > 0 || match t.result with Some (`Fail _, _) -> true | _ -> false
  then `Failed
  else if t.cancelled > 0 then `Cancelled
  else `Succeeded

(* On a terminal, the header's name gives way last: the time and the count
   go first. *)
let header t =
  Option.map
    (fun h ->
      let meta =
        Fmt.kstr (muted t) " %s (%d/%d)"
          (Display_text.duration (t.now () -. t.start))
          t.finished t.started
      in
      let lead = Span.(mark t (state t) ++ text " ") in
      let h = Span.styled (Theme.header (theme t)) h in
      let full = Span.(lead ++ h ++ meta) in
      if (not (is_live t)) || Span.width full <= t.width then full
      else cut t t.width Span.(lead ++ h))
    t.header

let verdict t result =
  let m, msg =
    match result with `Ok m -> (`Succeeded, m) | `Fail m -> (`Failed, m)
  in
  let lead = Span.(mark t m ++ text " ") in
  let msg = Span.styled Style.bold msg in
  if not (is_live t) then [ Span.(lead ++ msg) ]
  else
    let w = Span.width lead in
    match Span.wrap (max 1 (t.width - w)) msg with
    | first :: rest ->
        Span.(lead ++ first) :: List.map (fun l -> Span.(pad w ++ l)) rest
    | [] -> [ lead ]

let detail t lines =
  List.concat_map
    (fun l ->
      let l = muted t l in
      if is_live t then Span.wrap (max 1 t.width) l else [ l ])
    lines
