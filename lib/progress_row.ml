(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Task_core
module V = View_core

let text = V.text
let pad = Widget.pad

let pad_left n v =
  let w = V.width v in
  if w >= n then v else V.(pad (n - w) ++ v)

let theme (look : Look.t) = look.theme
let cut look w s = Widget.truncate look w s
let muted look s = V.ink (Widget.muted look) s

(* Successive rows take the colours of the theme's palette in turn. *)
let color look r =
  let palette = Theme.palette (theme look) in
  List.nth palette (abs (id r) mod List.length palette)

let mark_style look (m : Theme.mark) =
  let theme = theme look in
  match m with
  | `Succeeded -> Style.(bold + fg (Theme.succeeded theme))
  | `Failed | `Error -> Style.(bold + fg (Theme.failed theme))
  | `Cancelled | `Warning -> Style.(bold + fg (Theme.warning theme))
  | `Running -> Style.(bold + fg (Theme.accent theme))
  | `Info | `Debug -> Widget.muted look

let mark look m = V.ink (mark_style look m) (Widget.mark look m)
let outcome_mark (o : outcome) = (o :> Theme.mark)

(* {1 Live rows} *)

(* In floats: [100 * cur] overflows an int long before [cur] does. *)
let percent m =
  Option.map
    (fun f -> if f >= 1. then 100 else int_of_float (100. *. f))
    (fraction m)

(* No bar is drawn before work advances, since an empty track reads as an
   input field; its cells are kept blank, so that nothing moves when it
   shows. Once work has advanced, the bar shows the least it can. *)
let bar look r width =
  match fraction (metric r) with
  | Some f when f > 0. ->
      (* A half cell, the least a bar shows, in UTF-8 and in ASCII alike. *)
      let f = Float.max f (1. /. float (max 1 (width * 2))) in
      V.paint look (Bar.horizontal ~width f)
  | Some _ -> pad width
  | None -> V.empty

(* The count and rate columns are kept as wide as the widest count of bytes
   and the widest rate from the first frame, blank while unused, so that no
   column opens later. The kept cells give way after the rate and before
   the bar, to the columns the tasks use. *)
let kept_count = String.length "1023.99 KiB / 1023.99 TiB"
let kept_rate = String.length "1023.99 MiB/s"

let count_cell ?(kept = 0) look r =
  pad_left
    (max kept (Work.count_cell (work r)))
    (muted look (count_text (metric r)))

(* A count that gives way: items as they are, bytes in short units, and no
   column shared with the other rows. *)
let compact_count look r =
  match metric r with
  | Spin -> V.empty
  | Count _ -> muted look (count_text (metric r))
  | Bytes (cur, total) ->
      muted look (Task_text.bytes_short cur ^ "/" ^ Task_text.bytes_short total)

let timer look d =
  pad_left Task_text.duration_cells (muted look (Task_text.duration d))

let eta look r =
  match Task_core.eta r with
  | Some s -> muted look ("ETA " ^ Task_text.duration s)
  | None -> V.empty

(* A piece at [stage] of what gives way, the columns [kept] or not, or
   [None] for the label and the spacer, which the row lays out. *)
let piece look r ~time ~stage ~kept (p : Progress_layout.piece) =
  match p.kind with
  | Spinner ->
      Some (V.ink (Style.fg (color look r)) (Widget.spinner look ~time))
  | Label | Spacer -> None
  | Bar w -> Some (bar look r (Option.value w ~default:12))
  | Percent -> (
      match percent (metric r) with
      | Some p -> Some (Fmt.kstr (muted look) "%3d%%" p)
      | None -> Some V.empty)
  | Count ->
      Some
        (if stage >= 3 then compact_count look r
         else if kept then count_cell ~kept:kept_count look r
         else count_cell look r)
  | Rate ->
      let kept = if kept then kept_rate else 0 in
      Some
        (pad_left
           (max kept (Work.rate_cell (work r)))
           (muted look (rate_text (metric r) (rate r))))
  | Eta -> Some (eta look r)
  | Elapsed ->
      Some
        (if stage >= 3 then muted look (Task_text.duration (elapsed r))
         else timer look (elapsed r))
  | Const v -> Some (V.resolve (Ctx.v look) v)
  | Custom f -> Some (V.resolve (Ctx.v ~time look) (f r))

(* {2 What gives way}

   The label is cut only once the other pieces have given way, at their
   stages: by default 1 the rate, then the cells kept for the count and
   rate columns, 2 the bar and the percentage, 3 the
   count, which gets shorter and leaves the column it shares, 4 the elapsed
   time. *)

let last_stage pieces =
  List.fold_left
    (fun n (p : Progress_layout.piece) ->
      max n (Option.value p.stage ~default:0))
    3 pieces

let is_blank (p : Progress_layout.piece) =
  match p.kind with Const v -> String.trim (V.to_string v) = "" | _ -> false

let gone stage (p : Progress_layout.piece) =
  match p.stage with Some s -> stage >= s | None -> false

let is_rate (p : Progress_layout.piece) =
  match p.kind with Rate -> true | _ -> false

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
      (fun i p ->
        if gone stage p && not (is_rate p) then
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
   sit at the right edge. A bar of no width takes the slack before the
   spacer does. Without a room, nothing gives way. *)
let live look ~time ?width layout r =
  let layout = Progress_layout.pieces layout in
  let has k =
    List.exists (fun (p : Progress_layout.piece) -> p.kind = k) layout
  in
  let label = V.ink Style.bold (label r) in
  let want = if has Label then V.width label else 0 in
  let growing (p : Progress_layout.piece) = p.kind = Bar None in
  let reserve = if has Spacer then 1 else 0 in
  let at (stage, kept) =
    let pieces =
      List.map
        (fun p -> (p, piece look r ~time ~stage ~kept p))
        (left stage layout)
    in
    let fixed =
      List.fold_left
        (fun n (p, v) ->
          match v with Some v when not (growing p) -> n + V.width v | _ -> n)
        0 pieces
    in
    (pieces, fixed)
  in
  let width =
    Option.value width ~default:(want + reserve + snd (at (0, true)) + 12)
  in
  let steps =
    (0, true) :: (1, true)
    :: List.init (last_stage layout) (fun i -> (i + 1, false))
  in
  let rec fit = function
    | [] -> assert false
    | [ step ] -> at step
    | step :: rest ->
        let pieces, fixed = at step in
        if width - fixed - reserve >= want then (pieces, fixed) else fit rest
  in
  let pieces, fixed = fit steps in
  let room = width - fixed - reserve in
  let label =
    if not (has Label) then V.empty
    else if V.width label <= room then label
    else cut look room label
  in
  let slack = max 0 (width - fixed - V.width label) in
  let grows = List.exists (fun (p, _) -> growing p) pieces in
  let line =
    V.concat
      (List.map
         (fun ((p : Progress_layout.piece), v) ->
           match (p.kind, v) with
           | Label, _ -> label
           | Spacer, _ -> pad (if grows then reserve else max reserve slack)
           | Bar None, _ -> bar look r (max 1 (slack - reserve))
           | _, Some v -> v
           | _, None -> V.empty)
         pieces)
  in
  cut look width line

let gap_suffix look gap =
  if gap >= 1. then muted look ("  +" ^ Task_text.duration gap) else V.empty

(* A task's output after the mark of its kind; a Logs message after the
   mark of its level, none for an application or an information message. *)
let lead_mark look = function
  | Output -> V.(mark look `Info ++ text " ")
  | Logs (`App | `Info) -> V.empty
  | Logs ((`Warning | `Error | `Debug) as l) ->
      V.(mark look (l :> Theme.mark) ++ text " ")

let body look (line : line) = V.(text line.text ++ gap_suffix look line.gap)

let tail look r =
  Queue.fold
    (fun acc line ->
      (line.seq, V.(lead_mark look line.kind ++ body look line)) :: acc)
    [] (window r)
  |> List.rev

(* {1 Permanent lines} *)

let label_of r = Option.value (message r) ~default:(label r)

let mark_of look r =
  mark look (outcome_mark (Option.value (outcome r) ~default:`Cancelled))

(* [lead], then the lines of [body] broken to [width], each after the
   first under its start. *)
let hanging look ~width lead body =
  let w = V.width lead in
  (* Only a room narrower than the lead cuts a line. *)
  List.map (cut look width)
    (match V.wrap (max 1 (width - w)) body with
    | first :: rest ->
        V.(lead ++ first) :: List.map (fun l -> V.(pad w ++ l)) rest
    | [] -> [ lead ])

(* The count and the timer of a task that ended, at a stage of what gives
   way: 0 in the columns every task uses, 1 the count shorter and out of
   them, the timer too, 2 the count alone. *)
let meta look r stage =
  let timer =
    if stage = 0 then timer look (elapsed r)
    else muted look (Task_text.duration (elapsed r))
  in
  let count =
    if stage = 0 then
      if Work.count_cell (work r) = 0 then None else Some (count_cell look r)
    else
      let c = compact_count look r in
      if V.is_empty c then None else Some c
  in
  match (stage, count) with
  | (0 | 1), None -> timer
  | (0 | 1), Some c -> V.(c ++ text "  " ++ timer)
  | _, None -> V.empty
  | _, Some c -> c

let meta_stages = 2

(* [line] and [meta] against the right edge, two cells apart at least. *)
let against look ~width line meta =
  if V.is_empty meta then cut look width line
  else
    let gap = width - V.width line - V.width meta in
    cut look width V.(line ++ pad gap ++ meta)

(* A label is never wrapped in fewer cells than it keeps when it is cut. *)
let narrowest = 12

(* The mark and the label, then the count and the timer against the right
   edge, in the cells every task uses, so the columns line up whatever the
   labels. In the region, the count and the timer give way until the label
   fits, and only then is the label cut. With [whole], the label is never
   cut: it goes on over the lines below, at the full width left, and the
   count and the timer go at the end of its last line, or give way. Without
   a room, the line is whole. *)
let ended look ?width ~whole r =
  let lead = V.(mark_of look r ++ text " ") in
  let lead_w = V.width lead in
  let label = text (label_of r) in
  let label_w = V.width label in
  let full = meta look r 0 in
  let width =
    match width with Some w -> w | None -> lead_w + label_w + 2 + V.width full
  in
  let room m = width - lead_w - if V.is_empty m then 0 else V.width m + 2 in
  if room full >= label_w then [ against look ~width V.(lead ++ label) full ]
  else if not whole then
    let rec fit stage =
      let m = meta look r stage in
      if room m >= label_w || stage = meta_stages then m else fit (stage + 1)
    in
    let m = fit 0 in
    [ against look ~width V.(lead ++ cut look (room m) label) m ]
  else
    let lines =
      if width - lead_w >= min narrowest label_w then
        match V.wrap (max 1 (width - lead_w)) label with
        | first :: rest ->
            V.(lead ++ first) :: List.map (fun l -> V.(pad lead_w ++ l)) rest
        | [] -> [ lead ]
      else
        let indent = max 0 (min lead_w (width - narrowest)) in
        lead
        :: List.map
             (fun l -> V.(pad indent ++ l))
             (V.wrap (max 1 (width - indent)) label)
    in
    let rev = List.rev lines in
    let last = List.hd rev in
    let rec place stage =
      if stage > meta_stages then V.empty
      else
        let m = meta look r stage in
        if V.is_empty m || V.width last + 2 + V.width m <= width then m
        else place (stage + 1)
    in
    List.rev_map (cut look width)
      (against look ~width last (place 0) :: List.tl rev)

let dropped_line look n what =
  muted look
    (Fmt.str "%s %d %s dropped" (Widget.ellipsis look) n
       (if n = 1 then what else what ^ "s"))

let output look ~width r =
  let dropped =
    if dropped r = 0 then []
    else [ cut look width (dropped_line look (dropped r) "line") ]
  in
  dropped
  @ List.concat_map
      (fun line ->
        hanging look ~width (lead_mark look line.kind) (body look line))
      (List.rev (Queue.fold (fun acc l -> l :: acc) [] (output r)))

let finished look ~failed ~cancelled n (m : Theme.mark) =
  let some n what = if n = 0 then "" else Fmt.str ", %d %s" n what in
  V.(
    mark look m ++ text " "
    ++ muted look
         (Fmt.str "%d done%s%s" n (some failed "failed")
            (some cancelled "cancelled")))

let shed look n = dropped_line look n "task"

let more look n =
  muted look (Fmt.str "%s %d more running" (Widget.ellipsis look) n)

(* {2 Without a terminal}

   The lines of tasks that run at once interleave, so each names its task
   by its path, the labels from its top task down, rather than by an
   indentation that the line above may belong to another tree. *)

let separator look = Widget.glyph look " \u{25B8} " ~ascii:" > "

(* The labels above the task, muted, then [own]. *)
let path look r own =
  let above =
    match List.rev (path r) with _ :: up -> List.rev up | [] -> []
  in
  V.concat
    (List.map (fun l -> muted look (l ^ separator look)) above @ [ text own ])

(* A line of output is two columns in from the lines of tasks. *)
let under_task = text "  "

(* The same, compact, on one line, which says how a task that did not
   succeed ended, as no colour may. *)
let appended look r =
  let count = match count_text (metric r) with "" -> "" | c -> "  " ^ c in
  let ended =
    match Option.value (outcome r) ~default:`Cancelled with
    | `Succeeded -> ""
    | `Failed -> "  failed"
    | `Cancelled -> "  cancelled"
  in
  V.(
    mark_of look r ++ text " "
    ++ path look r (label_of r)
    ++ muted look (count ^ "  " ^ Task_text.duration (elapsed r) ^ ended))

(* In a room, a line that does not fit goes on over the lines below, under
   its start; without one it is a line of its own, as long as it is. *)
let folded look ?width lead body =
  match width with
  | None -> [ V.(lead ++ body) ]
  | Some width -> hanging look ~width lead body

(* The mark leads, so that a line that goes on over the next ones goes on
   under its text. *)
let labelled look ?width r line =
  folded look ?width
    V.(under_task ++ lead_mark look line.kind)
    V.(path look r (label r) ++ muted look ": " ++ body look line)

let under look ?width line =
  folded look ?width V.(under_task ++ lead_mark look line.kind) (body look line)

let dropped look n = V.(under_task ++ dropped_line look n "line")
let started look r = V.(mark look `Running ++ text " " ++ path look r (label r))

(* A line of the root is output of no task: a Logs message after its
   level's mark, and a line the program printed after none. *)
let message look ?width (line : line) =
  let kind = match line.kind with Output -> Logs `App | k -> k in
  folded look ?width (lead_mark look kind) (text line.text)

(* {1 The summary and the verdict} *)

(* The worst of how the work and its tasks ended. *)
let state root : Theme.mark =
  let w = work root in
  match outcome root with
  | None -> `Running
  | Some o ->
      if o = `Failed || Work.failed w > 0 then `Failed
      else if o = `Cancelled || Work.cancelled w > 0 then `Cancelled
      else `Succeeded

(* In a room, the summary's name gives way last: the time and the count go
   first. *)
let summary look ?width root =
  let w = work root in
  Option.map
    (fun h ->
      let meta =
        muted look
          (String.concat ""
             [
               " ";
               Task_text.duration (elapsed root);
               " (";
               string_of_int (Work.finished w);
               "/";
               string_of_int (Work.started w);
               ")";
             ])
      in
      let lead = V.(mark look (state root) ++ text " ") in
      let h = V.ink (Theme.header (theme look)) h in
      let full = V.(lead ++ h ++ meta) in
      match width with
      | Some width when V.width full > width -> cut look width V.(lead ++ h)
      | _ -> full)
    (Work.summary w)

let verdict look ?width root =
  match (outcome root, Task_core.message root) with
  | Some o, Some msg -> (
      let lead = V.(mark look (outcome_mark o) ++ text " ") in
      let msg = V.ink Style.bold msg in
      match width with
      | None -> [ V.(lead ++ msg) ]
      | Some width -> (
          let w = V.width lead in
          match V.wrap (max 1 (width - w)) msg with
          | first :: rest ->
              V.(lead ++ first) :: List.map (fun l -> V.(pad w ++ l)) rest
          | [] -> [ lead ]))
  | _ -> []
