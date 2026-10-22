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
  | `Info | `Debug -> Widget.muted (look t)

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
let guided t s = Span.styled (Guide.style (guide t)) s

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
      Span_core.resolve (look t) (Bar.to_span (Bar.v ~width f))
  | _ -> Span.empty

let count_cell t r = pad_left t.count_cell (muted t (count_text r.metric))

let timer t d =
  pad_left Display_text.duration_cells (muted t (Display_text.duration d))

let piece t r ~time (p : Display_layout.piece) =
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
let live t ~width ~lasts r =
  let time = t.now () -. t.start in
  let pieces =
    List.map (fun p -> (p, piece t r ~time p)) (Display_layout.pieces t.layout)
  in
  let prefix = prefix t lasts in
  let has p = List.exists (fun (q, _) -> q = p) pieces in
  let room = width in
  let fixed =
    Span.width (Span.text prefix)
    + List.fold_left (fun n (_, s) -> n + Span.width s) 0 pieces
  in
  let reserve = if has Display_layout.Spacer then 1 else 0 in
  let label =
    if has Display_layout.Label then
      cut t (room - fixed - reserve) (Span.styled Style.bold r.label)
    else Span.empty
  in
  let slack = max reserve (room - fixed - Span.width label) in
  let line =
    Span.concat
      (guided t prefix
      :: List.map
           (fun ((p : Display_layout.piece), s) ->
             match p with Label -> label | Spacer -> pad slack | _ -> s)
           pieces)
  in
  cut t room line

let gap_suffix t gap =
  if gap >= 1. then muted t ("  +" ^ Display_text.duration gap) else Span.empty

(* A task's output after the mark of its kind; a Logs message after the
   mark of its level, none for an application or an information message. *)
let lead_mark t = function
  | Output -> Span.(mark t `Info ++ text " ")
  | Logs (`App | `Info) -> Span.empty
  | Logs ((`Warning | `Error | `Debug) as l) ->
      Span.(mark t (l :> Theme.mark) ++ text " ")

let body t (line : line) = Span.(text line.text ++ gap_suffix t line.gap)

let tail t ~width ~lasts r =
  let prefix = guided t (continuation t lasts) in
  Queue.fold
    (fun acc line ->
      ( line.seq,
        cut t width Span.(prefix ++ lead_mark t line.kind ++ body t line) )
      :: acc)
    [] r.window
  |> List.rev

(* {1 Permanent lines} *)

let label_of r = Option.value r.message ~default:r.label

let mark_of t r =
  mark t (outcome_mark (Option.value r.outcome ~default:`Cancelled))

(* [lead], then the lines of [body] broken to [width], each after the
   first under its start. *)
let hanging t ~width lead body =
  let w = Span.width lead in
  (* Only a terminal narrower than the lead cuts a line. *)
  List.map (cut t width)
    (match Span.wrap (max 1 (width - w)) body with
    | first :: rest ->
        Span.(lead ++ first) :: List.map (fun l -> Span.(pad w ++ l)) rest
    | [] -> [ lead ])

(* The mark and the label, then the count and the timer against the right
   edge, in the cells every task uses, so the columns line up whatever the
   labels. A label that does not fit is cut, or with [whole] goes on over the
   lines below. *)
let ended t ~width ~lasts ~whole r =
  let lead = Span.(guided t (prefix t lasts) ++ mark_of t r ++ text " ") in
  let lead_w = Span.width lead in
  let timer = timer t (elapsed r) in
  let meta =
    if t.count_cell = 0 then timer
    else Span.(count_cell t r ++ text "  " ++ timer)
  in
  let meta = if lead_w + 2 + Span.width meta <= width then meta else timer in
  let space = width - lead_w - Span.width meta - 2 in
  let label = text (label_of r) in
  let first, rest =
    if space <= 0 then (Span.empty, if whole then [ label ] else [])
    else if not whole then (cut t space label, [])
    else
      match Span.wrap space label with
      | first :: rest -> (first, rest)
      | [] -> (Span.empty, [])
  in
  let head = Span.(lead ++ first) in
  let gap = width - Span.width head - Span.width meta in
  let head = cut t width Span.(head ++ pad gap ++ meta) in
  let under = pad lead_w in
  head
  :: List.concat_map
       (fun l ->
         List.map
           (fun l -> cut t width Span.(under ++ l))
           (Span.wrap (max 1 (width - lead_w)) l))
       rest

let output t ~width ~lasts r =
  let lead = guided t (continuation t lasts) in
  let dropped =
    if r.dropped = 0 then []
    else
      [
        cut t width
          Span.(
            lead
            ++ muted t
                 (Fmt.str "%s %d %s dropped"
                    (Widget.ellipsis (look t))
                    r.dropped
                    (if r.dropped = 1 then "line" else "lines")));
      ]
  in
  dropped
  @ List.concat_map
      (fun line ->
        hanging t ~width Span.(lead ++ lead_mark t line.kind) (body t line))
      (List.rev (Queue.fold (fun acc l -> l :: acc) [] r.output))

let finished t ~width ~lasts n (m : Theme.mark) =
  cut t width
    Span.(
      guided t (prefix t lasts)
      ++ mark t m ++ text " "
      ++ muted t (Fmt.str "%d done" n))

let shed t ~width ~lasts n =
  cut t width
    Span.(
      guided t (prefix t lasts)
      ++ muted t
           (Fmt.str "%s %d %s dropped"
              (Widget.ellipsis (look t))
              n
              (if n = 1 then "task" else "tasks")))

let more t ~width n =
  cut t width
    (muted t (Fmt.str "  %s %d more running" (Widget.ellipsis (look t)) n))

(* {2 Without a terminal}

   The lines of tasks that run at once interleave, so each names its task
   by its path, the labels from its top task down, rather than by an
   indentation that the line above may belong to another tree. *)

let ancestors r =
  let rec up acc r =
    match r.parent with None -> acc | Some p -> up (p.label :: acc) p
  in
  up [] r

let separator t = Widget.glyph (look t) " \u{203A} " ~ascii:" > "

(* The labels above the task, muted, then [own]. *)
let path t r own =
  Span.concat
    (List.map (fun l -> muted t (l ^ separator t)) (ancestors r) @ [ text own ])

(* A line of output is two columns in from the lines of tasks. *)
let under_task = text "  "

(* The same, compact, on one line, which says how a task that did not
   succeed ended, as no colour may. *)
let appended r =
  let t = r.disp in
  let count = match count_text r.metric with "" -> "" | c -> "  " ^ c in
  let ended =
    match Option.value r.outcome ~default:`Cancelled with
    | `Succeeded -> ""
    | `Failed -> "  failed"
    | `Cancelled -> "  cancelled"
  in
  Span.(
    mark_of t r ++ text " "
    ++ path t r (label_of r)
    ++ muted t (count ^ "  " ^ Display_text.duration (elapsed r) ^ ended))

(* On a terminal a line that does not fit goes on over the lines below,
   under its start; without one it is a line of its own, as long as it is. *)
let folded t ~width lead body =
  if not (is_live t) then [ Span.(lead ++ body) ]
  else hanging t ~width lead body

(* The mark leads, so that a line that goes on over the next ones goes on
   under its text. *)
let labelled r line =
  let t = r.disp in
  folded t ~width:(room t)
    Span.(under_task ++ lead_mark t line.kind)
    Span.(path t r r.label ++ muted t ": " ++ body t line)

let under r line =
  let t = r.disp in
  folded t ~width:(room t)
    Span.(under_task ++ lead_mark t line.kind)
    (body t line)

let dropped r n =
  let t = r.disp in
  let look = look t in
  Span.(
    under_task
    ++ muted t
         (Fmt.str "%s %d %s dropped" (Widget.ellipsis look) n
            (if n = 1 then "line" else "lines")))

let started r =
  let t = r.disp in
  Span.(mark t `Running ++ text " " ++ path t r r.label)

let message t level msg =
  folded t ~width:t.width (lead_mark t (Logs level)) (text msg)

(* {1 The summary and the verdict} *)

let state t : Theme.mark =
  if (not t.closed) && (t.started = 0 || t.finished < t.started) then `Running
  else if
    t.failed > 0 || match t.result with Some (Error _, _) -> true | _ -> false
  then `Failed
  else if t.cancelled > 0 then `Cancelled
  else `Succeeded

(* On a terminal, the summary's name gives way last: the time and the count
   go first. *)
let summary t =
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
      if (not (is_live t)) || Span.width full <= room t then full
      else cut t (room t) Span.(lead ++ h))
    t.summary

let verdict t result =
  let m, msg =
    match result with Ok m -> (`Succeeded, m) | Error m -> (`Failed, m)
  in
  let lead = Span.(mark t m ++ text " ") in
  let msg = Span.styled Style.bold msg in
  if not (is_live t) then [ Span.(lead ++ msg) ]
  else
    let w = Span.width lead in
    match Span.wrap (max 1 (room t - w)) msg with
    | first :: rest ->
        Span.(lead ++ first) :: List.map (fun l -> Span.(pad w ++ l)) rest
    | [] -> [ lead ]

let detail t lines =
  List.concat_map
    (fun l ->
      let l = muted t l in
      if is_live t then Span.wrap (max 1 (room t)) l else [ l ])
    lines
