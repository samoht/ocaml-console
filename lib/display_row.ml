(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let text = View_core.text
let pad = Widget.pad

let pad_left n span =
  let w = View_core.width span in
  if w >= n then span else View_core.(pad (n - w) ++ span)

(* The display is drawn in the look of its formatter. *)
let look t = Look.v t.ppf
let theme t = (look t).theme
let cut t w s = Widget.truncate (look t) w s
let muted t s = View_core.ink (Widget.muted (look t)) s

(* Successive rows take the colours of the theme's palette in turn. *)
let color r =
  let palette = Theme.palette (theme r.disp) in
  List.nth palette (r.id mod List.length palette)

let mark_style t (m : Theme.mark) =
  let theme = theme t in
  match m with
  | `Succeeded -> Style.(bold + fg (Theme.succeeded theme))
  | `Failed | `Error -> Style.(bold + fg (Theme.failed theme))
  | `Cancelled | `Warning -> Style.(bold + fg (Theme.warning theme))
  | `Running -> Style.(bold + fg (Theme.accent theme))
  | `Info | `Debug -> Widget.muted (look t)

let mark t m = View_core.ink (mark_style t m) (Widget.mark (look t) m)
let outcome_mark (o : outcome) = (o :> Theme.mark)

(* {1 Guides} *)

let guide t = Widget.guide (look t) None

(* [lasts] are a row's flags, innermost first, ending with its root's, which
   draws nothing: a root row starts at the left edge. *)
let rec nested = function [] | [ _ ] -> [] | own :: up -> own :: nested up

let prefix t lasts =
  match nested lasts with [] -> "" | l -> "  " ^ Widget.branch (guide t) l

let continuation t lasts = "  " ^ Widget.rails (guide t) (nested lasts)
let guided t s = View_core.ink (Guide.style (guide t)) s

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
      View_core.paint (look t) (Bar.horizontal ~width f)
  | _ -> View_core.empty

let count_cell t r = pad_left t.count_cell (muted t (count_text r.metric))

(* A count that gives way: items as they are, bytes in short units, and no
   column shared with the other rows. *)
let compact_count t r =
  match r.metric with
  | Spin -> View_core.empty
  | Count _ -> muted t (count_text r.metric)
  | Bytes (cur, total) ->
      muted t
        (Display_text.bytes_short cur ^ "/" ^ Display_text.bytes_short total)

let timer t d =
  pad_left Display_text.duration_cells (muted t (Display_text.duration d))

let piece t r ~time (p : Display_layout.piece) =
  match p with
  | Spinner ->
      View_core.ink (Style.fg (color r)) (Widget.spinner (look t) ~time)
  | Label | Spacer -> View_core.empty
  | Bar n -> bar t r n
  | Percent -> (
      match percent r.metric with
      | Some p -> Fmt.kstr (muted t) "%3d%%" p
      | None -> View_core.empty)
  | Count -> count_cell t r
  | Rate -> pad_left t.rate_cell (muted t (rate_text r.metric r.rate))
  | Elapsed -> timer t (elapsed r)
  | Text s -> text s
  | Accent s -> View_core.ink Style.(bold + fg (color r)) s

(* {2 What gives way}

   The label is cut only once the other pieces have given way, at stages:
   1 the rate, 2 the bar and the percentage, 3 the count, which gets
   shorter and leaves the column it shares, 4 the elapsed time. *)

let last_stage = 4

let is_blank (p : Display_layout.piece) =
  match p with Text s -> String.trim s = "" | _ -> false

let gone stage (p : Display_layout.piece) =
  match p with
  | Rate -> stage >= 1
  | Bar _ | Percent -> stage >= 2
  | Elapsed -> stage >= 4
  | _ -> false

(* The pieces left at [stage], each gone piece with the blank text before
   it, or after it if none is before. The rate leaves its blanks, so that a
   count keeps its distance to the timer, as on the line of a task that
   ended. A row whose last piece has gone ends with no blank. *)
let left stage pieces =
  if stage = 0 then pieces
  else
    let a = Array.of_list pieces in
    let n = Array.length a in
    let keep = Array.map (fun p -> not (gone stage p)) a in
    Array.iteri
      (fun i (p : Display_layout.piece) ->
        if gone stage p && p <> Rate then
          if i > 0 && keep.(i - 1) && is_blank a.(i - 1) then
            keep.(i - 1) <- false
          else if i + 1 < n && keep.(i + 1) && is_blank a.(i + 1) then
            keep.(i + 1) <- false)
      a;
    if n > 0 && gone stage a.(n - 1) then begin
      let rec trim i =
        if i >= 0 && ((not keep.(i)) || is_blank a.(i)) then begin
          keep.(i) <- false;
          trim (i - 1)
        end
      in
      trim (n - 1)
    end;
    List.filteri (fun i _ -> keep.(i)) pieces

(* The label takes the room the other pieces leave, and they give way
   until it fits whole; one spacer takes the slack, so the pieces after it
   sit at the right edge. *)
let live t ~width ~lasts r =
  let time = since_start t in
  let layout = Display_layout.pieces t.layout in
  let prefix = prefix t lasts in
  let has p = List.mem p layout in
  let label = View_core.ink Style.bold r.label in
  let want = if has Display_layout.Label then View_core.width label else 0 in
  let reserve = if has Display_layout.Spacer then 1 else 0 in
  let at stage =
    let pieces =
      List.map
        (fun (p : Display_layout.piece) ->
          match p with
          | Count when stage >= 3 -> (p, compact_count t r)
          | Elapsed when stage >= 3 ->
              (p, muted t (Display_text.duration (elapsed r)))
          | p -> (p, piece t r ~time p))
        (left stage layout)
    in
    let fixed =
      View_core.width (View_core.text prefix)
      + List.fold_left (fun n (_, s) -> n + View_core.width s) 0 pieces
    in
    (pieces, fixed)
  in
  let rec fit stage =
    let pieces, fixed = at stage in
    if width - fixed - reserve >= want || stage = last_stage then (pieces, fixed)
    else fit (stage + 1)
  in
  let pieces, fixed = fit 0 in
  let room = width - fixed - reserve in
  let label =
    if want = 0 && not (has Display_layout.Label) then View_core.empty
    else if View_core.width label <= room then label
    else cut t room label
  in
  let slack = max reserve (width - fixed - View_core.width label) in
  let line =
    View_core.concat
      (guided t prefix
      :: List.map
           (fun ((p : Display_layout.piece), s) ->
             match p with Label -> label | Spacer -> pad slack | _ -> s)
           pieces)
  in
  cut t width line

let gap_suffix t gap =
  if gap >= 1. then muted t ("  +" ^ Display_text.duration gap)
  else View_core.empty

(* A task's output after the mark of its kind; a Logs message after the
   mark of its level, none for an application or an information message. *)
let lead_mark t = function
  | Output -> View_core.(mark t `Info ++ text " ")
  | Logs (`App | `Info) -> View_core.empty
  | Logs ((`Warning | `Error | `Debug) as l) ->
      View_core.(mark t (l :> Theme.mark) ++ text " ")

let body t (line : line) = View_core.(text line.text ++ gap_suffix t line.gap)

let tail t ~width ~lasts r =
  let prefix = guided t (continuation t lasts) in
  Queue.fold
    (fun acc line ->
      ( line.seq,
        cut t width View_core.(prefix ++ lead_mark t line.kind ++ body t line)
      )
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
  let w = View_core.width lead in
  (* Only a terminal narrower than the lead cuts a line. *)
  List.map (cut t width)
    (match View_core.wrap (max 1 (width - w)) body with
    | first :: rest ->
        View_core.(lead ++ first)
        :: List.map (fun l -> View_core.(pad w ++ l)) rest
    | [] -> [ lead ])

(* The count and the timer of a task that ended, at a stage of what gives
   way: 0 in the columns every task uses, 1 the count shorter and out of
   them, the timer too, 2 the count alone. *)
let meta t r stage =
  let timer =
    if stage = 0 then timer t (elapsed r)
    else muted t (Display_text.duration (elapsed r))
  in
  let count =
    if stage = 0 then if t.count_cell = 0 then None else Some (count_cell t r)
    else
      let c = compact_count t r in
      if View_core.is_empty c then None else Some c
  in
  match (stage, count) with
  | (0 | 1), None -> timer
  | (0 | 1), Some c -> View_core.(c ++ text "  " ++ timer)
  | _, None -> View_core.empty
  | _, Some c -> c

let meta_stages = 2

(* [line] and [meta] against the right edge, two cells apart at least. *)
let against t ~width line meta =
  if View_core.is_empty meta then cut t width line
  else
    let gap = width - View_core.width line - View_core.width meta in
    cut t width View_core.(line ++ pad gap ++ meta)

(* A label is never wrapped in fewer cells than it keeps when it is cut. *)
let narrowest = 12

(* The mark and the label, then the count and the timer against the right
   edge, in the cells every task uses, so the columns line up whatever the
   labels. In the region, the count and the timer give way until the label
   fits, and only then is the label cut. With [whole], the label is never
   cut: it goes on over the lines below, at the full width left, and the
   count and the timer go at the end of its last line, or give way. *)
let ended t ~width ~lasts ~whole r =
  let lead = View_core.(guided t (prefix t lasts) ++ mark_of t r ++ text " ") in
  let lead_w = View_core.width lead in
  let label = text (label_of r) in
  let label_w = View_core.width label in
  let room m =
    width - lead_w - if View_core.is_empty m then 0 else View_core.width m + 2
  in
  let full = meta t r 0 in
  if room full >= label_w then
    [ against t ~width View_core.(lead ++ label) full ]
  else if not whole then
    let rec fit stage =
      let m = meta t r stage in
      if room m >= label_w || stage = meta_stages then m else fit (stage + 1)
    in
    let m = fit 0 in
    [ against t ~width View_core.(lead ++ cut t (room m) label) m ]
  else
    let lines =
      if width - lead_w >= min narrowest label_w then
        match View_core.wrap (max 1 (width - lead_w)) label with
        | first :: rest ->
            View_core.(lead ++ first)
            :: List.map (fun l -> View_core.(pad lead_w ++ l)) rest
        | [] -> [ lead ]
      else
        let indent = max 0 (min lead_w (width - narrowest)) in
        lead
        :: List.map
             (fun l -> View_core.(pad indent ++ l))
             (View_core.wrap (max 1 (width - indent)) label)
    in
    let rev = List.rev lines in
    let last = List.hd rev in
    let rec place stage =
      if stage > meta_stages then View_core.empty
      else
        let m = meta t r stage in
        if
          View_core.is_empty m
          || View_core.width last + 2 + View_core.width m <= width
        then m
        else place (stage + 1)
    in
    List.rev_map (cut t width) (against t ~width last (place 0) :: List.tl rev)

let output t ~width ~lasts r =
  let lead = guided t (continuation t lasts) in
  let dropped =
    if r.dropped = 0 then []
    else
      [
        cut t width
          View_core.(
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
        hanging t ~width View_core.(lead ++ lead_mark t line.kind) (body t line))
      (List.rev (Queue.fold (fun acc l -> l :: acc) [] r.output))

let finished t ~width ~lasts ~failed ~cancelled n (m : Theme.mark) =
  let some n what = if n = 0 then "" else Fmt.str ", %d %s" n what in
  cut t width
    View_core.(
      guided t (prefix t lasts)
      ++ mark t m ++ text " "
      ++ muted t
           (Fmt.str "%d done%s%s" n (some failed "failed")
              (some cancelled "cancelled")))

let shed t ~width ~lasts n =
  cut t width
    View_core.(
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

let separator t = Widget.glyph (look t) " \u{25B8} " ~ascii:" > "

(* The labels above the task, muted, then [own]. *)
let path t r own =
  View_core.concat
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
  View_core.(
    mark_of t r ++ text " "
    ++ path t r (label_of r)
    ++ muted t (count ^ "  " ^ Display_text.duration (elapsed r) ^ ended))

(* On a terminal a line that does not fit goes on over the lines below,
   under its start; without one it is a line of its own, as long as it is. *)
let folded t ~width lead body =
  if not (is_live t) then [ View_core.(lead ++ body) ]
  else hanging t ~width lead body

(* The mark leads, so that a line that goes on over the next ones goes on
   under its text. *)
let labelled r line =
  let t = r.disp in
  folded t ~width:(room t)
    View_core.(under_task ++ lead_mark t line.kind)
    View_core.(path t r r.label ++ muted t ": " ++ body t line)

let under r line =
  let t = r.disp in
  folded t ~width:(room t)
    View_core.(under_task ++ lead_mark t line.kind)
    (body t line)

let dropped r n =
  let t = r.disp in
  let look = look t in
  View_core.(
    under_task
    ++ muted t
         (Fmt.str "%s %d %s dropped" (Widget.ellipsis look) n
            (if n = 1 then "line" else "lines")))

let started r =
  let t = r.disp in
  View_core.(mark t `Running ++ text " " ++ path t r r.label)

let message t level msg =
  folded t ~width:t.width (lead_mark t (Logs level)) (text msg)

(* {1 The summary and the verdict} *)

let state t : Theme.mark =
  if (not t.closed) && Option.is_none t.result then `Running
  else if
    t.failed > 0 || match t.result with Some (Error _, _) -> true | _ -> false
  then `Failed
  else if t.cancelled > 0 then `Cancelled
  else `Succeeded

(* On a terminal, the summary's name gives way last: the time and the count
   go first. *)
let summary ?width t =
  let width = Option.value width ~default:(room t) in
  Option.map
    (fun h ->
      let meta =
        Fmt.kstr (muted t) " %s (%d/%d)"
          (Display_text.duration (since_start t))
          t.finished t.started
      in
      let lead = View_core.(mark t (state t) ++ text " ") in
      let h = View_core.ink (Theme.header (theme t)) h in
      let full = View_core.(lead ++ h ++ meta) in
      if (not (is_live t)) || View_core.width full <= width then full
      else cut t width View_core.(lead ++ h))
    t.summary

let verdict t result =
  let m, msg =
    match result with Ok m -> (`Succeeded, m) | Error m -> (`Failed, m)
  in
  let lead = View_core.(mark t m ++ text " ") in
  let msg = View_core.ink Style.bold msg in
  if not (is_live t) then [ View_core.(lead ++ msg) ]
  else
    let w = View_core.width lead in
    match View_core.wrap (max 1 (room t - w)) msg with
    | first :: rest ->
        View_core.(lead ++ first)
        :: List.map (fun l -> View_core.(pad w ++ l)) rest
    | [] -> [ lead ]

let detail t lines =
  List.concat_map
    (fun l ->
      let l = muted t l in
      if is_live t then View_core.wrap (max 1 (room t)) l else [ l ])
    lines
