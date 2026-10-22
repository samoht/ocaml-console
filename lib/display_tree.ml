(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

(* A transient task that ended has no line: the tasks under it take its
   place. *)
let shows r = is_running r || not r.transient

type node = { row : row; kids : node list }

(* The tasks [keep] keeps among [rows] and under them, each under the
   nearest of its parents kept. *)
let rec forest keep rows =
  List.concat_map
    (fun r ->
      let kids = forest keep (children r) in
      if keep r then [ { row = r; kids } ] else kids)
    rows

let rec size nodes = List.fold_left (fun n x -> n + 1 + size x.kids) 0 nodes

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

(* A line of a window gives way before the lines of tasks. *)
type item = Line of Span.t | Tail of Span.t

let line = function Line s | Tail s -> s

(* With [fold], the tasks that ended among [nodes], apart. *)
let split ~fold nodes =
  if fold then List.partition (fun n -> not (is_running n.row)) nodes
  else ([], nodes)

(* The running tasks alone count no task dropped. *)
let shed form r = if form = Bare then 0 else r.shed

(* The lines of [nodes], under a task whose guides [up] draws. A line that
   counts the tasks dropped, then the one that folds those that ended, come
   first. *)
let rec block t ~width ~form ~fold ~shed up nodes =
  let ended, shown = split ~fold nodes in
  let entries =
    (if shed > 0 then [ `Shed shed ] else [])
    @ (match ended with [] -> [] | ns -> [ `Folded ns ])
    @ List.map (fun n -> `Node n) shown
  in
  let k = List.length entries in
  List.concat
    (List.mapi
       (fun i e ->
         let lasts = (i = k - 1) :: up in
         match e with
         | `Shed n -> [ Line (Display_row.shed t ~width ~lasts n) ]
         | `Folded ns ->
             [
               Line
                 (Display_row.finished t ~width ~lasts (size ns)
                    (worst `Succeeded ns));
             ]
         | `Node n -> node t ~width ~form lasts n)
       entries)

and node t ~width ~form lasts n =
  let r = n.row in
  let own =
    if is_running r then
      Line (Display_row.live t ~width ~lasts r)
      ::
      (match form with
      | Full | Fold ->
          List.map (fun s -> Tail s) (Display_row.tail t ~width ~lasts r)
      | Bare | Whole -> [])
    else
      let whole = form = Whole in
      List.map (fun s -> Line s) (Display_row.ended t ~width ~lasts ~whole r)
      @
      if whole then
        List.map (fun s -> Line s) (Display_row.output t ~width ~lasts r)
      else []
  in
  own
  @ block t ~width ~form ~fold:(form = Fold) ~shed:(shed form r) lasts n.kids

(* The root draws no line: its tree is those of the top tasks, which fold as
   its children. A task's tree starts with its own line. *)
let tops keep r = forest keep (if is_root r then children r else [ r ])
let top_fold form r = form = Fold && is_root r
let top_shed form r = if is_root r then shed form r else 0

let items t ~width ~form r nodes =
  block t ~width ~form ~fold:(top_fold form r) ~shed:(top_shed form r) [] nodes

(* The lines [items] gives in the region, and how many are of windows,
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

let rec drop_tails n = function
  | [] -> []
  | Tail _ :: rest when n > 0 -> drop_tails (n - 1) rest
  | i :: rest -> i :: drop_tails n rest

(* The latest running tasks that fit, after a line that counts the others,
   the tree's own task first. *)
let bare t ~width ~height r =
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
  let body = List.map line (items t ~width ~form:Bare r (tops keep r)) in
  let body =
    if hidden = 0 then body
    else
      let more = Display_row.more t ~width hidden in
      match body with
      | first :: rest when top = 1 -> first :: more :: rest
      | body -> more :: body
  in
  List.filteri (fun i _ -> i < height) body

(* The tree is counted before it is drawn, and drawn once, in the form that
   fits. *)
let region t ~width ~height r =
  if height <= 0 || width <= 0 || r.gone then []
  else
    let nodes = tops shows r in
    if fst (count ~form:Full r nodes) <= height then
      List.map line (items t ~width ~form:Full r nodes)
    else
      let n, tails = count ~form:Fold r nodes in
      if n - tails > height then bare t ~width ~height r
      else
        let folded = items t ~width ~form:Fold r nodes in
        List.map line (drop_tails (n - height) folded)

let scrollback t ~width r =
  if width <= 0 || r.gone then []
  else List.map line (items t ~width ~form:Whole r (tops shows r))

let tree_lines ?width ?(height = max_int) r =
  let t = r.disp in
  let width = match width with Some w -> w | None -> room t in
  if t.closing then scrollback t ~width r else region t ~width ~height r

let unlines lines = Span_core.concat ~sep:(Span_core.text "\n") lines
let tree ?width ?height r = unlines (tree_lines ?width ?height r)

let default_lines t ~height =
  match Display_row.summary t with
  | None -> tree_lines ~height (root t)
  | Some summary -> tree_lines ~height:(height - 1) (root t) @ [ summary ]

let summary t = Option.value (Display_row.summary t) ~default:Span_core.empty
let default_view t ~width:_ ~height = unlines (default_lines t ~height)
