(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Task_core
module V = View_core

let live _ = V.moving

(* A label drawn where the tree lays it out: in its look, at its moment, in
   the room its guides leave it. *)
let label f =
  V.deferred ~kind:live (fun c ->
      f (Ctx.look c) ~time:(Ctx.time c) (Ctx.width c))

(* A line of the tree that is not a task's, cut to its room. *)
let line f =
  label (fun look ~time:_ width ->
      let l = f look in
      match width with Some w -> Widget.truncate look w l | None -> l)

(* A transient task that succeeded has no line: the tasks under it take
   its place. *)
let shows r = is_running r || (not r.transient) || r.outcome <> Some `Succeeded

type node = { row : t; kids : node list }

(* The tasks [keep] keeps among [rows] and under them, each under the
   nearest of its parents kept. *)
let rec forest keep rows =
  List.concat_map
    (fun r ->
      let kids = forest keep (children r) in
      if keep r then [ { row = r; kids } ] else kids)
    rows

let rec size nodes = List.fold_left (fun n x -> n + 1 + size x.kids) 0 nodes

(* The tasks among [nodes] and under them that ended with [outcome]. *)
let rec ending outcome nodes =
  List.fold_left
    (fun n x ->
      n
      + (if x.row.outcome = Some outcome then 1 else 0)
      + ending outcome x.kids)
    0 nodes

let rec worst acc nodes =
  List.fold_left
    (fun acc n ->
      let acc : Theme.mark =
        match (n.row.outcome, acc) with
        | Some `Failed, _ | _, `Failed -> `Failed
        | Some `Cancelled, _ | _, `Cancelled -> `Cancelled
        | _ -> acc
      in
      worst acc n.kids)
    acc nodes

(* How a tree is drawn: every task and window; the tasks that ended folded
   under each task; the running tasks alone; or as the scrollback keeps
   it. *)
type form = Full | Fold | Bare | Whole

(* With [fold], the tasks that ended among [nodes], apart. *)
let split ~fold nodes =
  if fold then List.partition (fun n -> not (is_running n.row)) nodes
  else ([], nodes)

(* The running tasks alone count no task dropped. *)
let shed form r = if form = Bare then 0 else r.shed

(* The label of a node: a running task's row, with the lines of its window
   newer than [cut] in the full and folded forms, and [extra] lines; a task
   that ended, its line, with its output when whole. *)
(* The lines under a top task's row are two cells further in than the row,
   behind the tree's indent, so they have two cells less. *)
let own ~layout ~form ~cut ~top ?(extra = []) r =
  let under width =
    Option.map (fun w -> max 1 (w - if top then 2 else 0)) width
  in
  if is_running r then
    label (fun look ~time width ->
        let tail =
          match form with
          | Full | Fold ->
              List.filter_map
                (fun (seq, l) ->
                  if seq > cut then
                    Some
                      (match under width with
                      | Some w -> Widget.truncate look w l
                      | None -> l)
                  else None)
                (Progress_row.tail look r)
          | Bare | Whole -> []
        in
        V.concat ~sep:(V.text "\n")
          ((Progress_row.live look ~time ?width layout r :: tail)
          @ List.map
              (fun f ->
                let l = f look in
                match under width with
                | Some w -> Widget.truncate look w l
                | None -> l)
              extra))
  else
    label (fun look ~time:_ width ->
        let whole = form = Whole in
        let lines = Progress_row.ended look ?width ~whole r in
        let output =
          if whole then
            Progress_row.output look
              ~width:(Option.value (under width) ~default:max_int)
              r
          else []
        in
        V.concat ~sep:(V.text "\n") (lines @ output))

(* The trees of [nodes], under a task: a line that counts the tasks
   dropped, then the one that folds those that ended, come first. *)
let rec block ?(top = false) ~layout ~form ~fold ~shed ~cut nodes =
  let ended, shown = split ~fold nodes in
  (if shed > 0 then
     [ Tree.v (line (fun look -> Progress_row.shed look shed)) [] ]
   else [])
  @ (match ended with
    | [] -> []
    | ns ->
        [
          Tree.v
            (line (fun look ->
                 Progress_row.finished look ~failed:(ending `Failed ns)
                   ~cancelled:(ending `Cancelled ns) (size ns)
                   (worst `Succeeded ns)))
            [];
        ])
  @ List.map (fun n -> node ~top ~layout ~form ~cut n) shown

and node ?(top = false) ~layout ~form ~cut ?extra n =
  let r = n.row in
  Tree.v
    (own ~layout ~form ~cut ~top ?extra r)
    (block ~layout ~form ~fold:(form = Fold) ~shed:(shed form r) ~cut n.kids)

(* The root draws no line: its tree is those of the top tasks, which fold as
   its children. A task's tree starts with its own line. *)
let tops keep r = forest keep (if is_root r then children r else [ r ])
let top_fold form r = form = Fold && is_root r
let top_shed form r = if is_root r then shed form r else 0

let trees ~layout ~form ~cut r nodes =
  block ~top:true ~layout ~form ~fold:(top_fold form r) ~shed:(top_shed form r)
    ~cut nodes

(* The lines the trees take in the region, and how many are of windows,
   counted without drawing them. *)
let rec count_block ~form ~fold ~shed nodes =
  let ended, shown = split ~fold nodes in
  let own = (if shed > 0 then 1 else 0) + if ended = [] then 0 else 1 in
  List.fold_left
    (fun (lines, tails) n ->
      let l, w = count_node ~form n in
      (lines + l, tails + w))
    (own, 0) shown

and count_node ~form n =
  let r = n.row in
  let window = if is_running r then Queue.length r.window else 0 in
  let lines, tails =
    count_block ~form ~fold:(form = Fold) ~shed:(shed form r) n.kids
  in
  (1 + window + lines, window + tails)

let count ~form r nodes =
  count_block ~form ~fold:(top_fold form r) ~shed:(top_shed form r) nodes

(* The seq of the newest of the [n] oldest lines of the windows of the
   running tasks, which give way. *)
let cut_tails n nodes =
  let rec seqs acc nodes =
    List.fold_left
      (fun acc x ->
        let acc =
          if is_running x.row then
            Queue.fold (fun acc (l : line) -> l.seq :: acc) acc x.row.window
          else acc
        in
        seqs acc x.kids)
      acc nodes
  in
  match List.sort Int.compare (seqs [] nodes) with
  | [] -> -1
  | sorted ->
      let n = min n (List.length sorted) in
      if n <= 0 then -1 else List.nth sorted (n - 1)

let draw trees = V.vcat (List.map (Tree.view ~indent:2) trees)

(* The first [height] lines of [v] laid out in [c]. *)
let first c height v =
  let v = V.resolve c v in
  if V.height v <= height then v
  else
    V.concat ~sep:(V.text "\n")
      (List.filteri (fun i _ -> i < height) (V.lines v))

(* The latest running tasks that fit, after a line that counts the others,
   the tree's own task first. *)
let bare ~layout c ~height r =
  let rec collect acc nodes =
    List.fold_left
      (fun acc n ->
        let acc =
          if is_running n.row && n.row != r then n.row :: acc else acc
        in
        collect acc n.kids)
      acc nodes
  in
  let running =
    collect [] (forest shows (if is_root r then children r else [ r ]))
  in
  let top = if is_root r then 0 else 1 in
  let room = height - top and n = List.length running in
  let shown = Hashtbl.create 16 in
  let hidden =
    if n <= room then begin
      List.iter (fun x -> Hashtbl.replace shown x.id ()) running;
      0
    end
    else
      let k = max 0 (room - 1) in
      List.sort (fun a b -> Int.compare b.id a.id) running
      |> List.iteri (fun i x -> if i < k then Hashtbl.replace shown x.id ());
      n - k
  in
  let keep x = x == r || Hashtbl.mem shown x.id in
  let nodes = tops keep r in
  let trees =
    if hidden = 0 then trees ~layout ~form:Bare ~cut:(-1) r nodes
    else if top = 1 then
      match nodes with
      | n :: rest ->
          node ~top:true ~layout ~form:Bare ~cut:(-1)
            ~extra:[ (fun look -> Progress_row.more look hidden) ]
            n
          :: trees ~layout ~form:Bare ~cut:(-1) r rest
      | [] -> []
    else
      Tree.v
        (line (fun look -> V.(text "  " ++ Progress_row.more look hidden)))
        []
      :: trees ~layout ~form:Bare ~cut:(-1) r nodes
  in
  first c height (draw trees)

(* The tree is counted before it is drawn, and drawn once, in the form that
   fits. *)
let region ~layout c ~height r =
  let width = Option.value (Ctx.width c) ~default:1 in
  if height <= 0 || width <= 0 || r.gone then V.empty
  else
    let nodes = tops shows r in
    if fst (count ~form:Full r nodes) <= height then
      draw (trees ~layout ~form:Full ~cut:(-1) r nodes)
    else
      let n, tails = count ~form:Fold r nodes in
      if n - tails > height then bare ~layout c ~height r
      else
        let cut = cut_tails (n - height) nodes in
        draw (trees ~layout ~form:Fold ~cut r nodes)

let whole ~layout r =
  if r.gone then V.empty
  else draw (trees ~layout ~form:Whole ~cut:(-1) r (tops shows r))

let view ~layout r =
  V.deferred ~kind:live (fun c ->
      match Ctx.height c with
      | None -> whole ~layout r
      | Some height -> region ~layout c ~height r)
