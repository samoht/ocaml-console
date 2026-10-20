(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metric = Spin | Count of int * int | Bytes of int64 * int64
type outcome = [ `Succeeded | `Failed | `Cancelled ]
type level = [ `Info | `Warning | `Error ]
type log = { gap : float; level : level; message : string; kept : bool }

let count_text = function
  | Spin -> ""
  | Count (c, tot) -> string_of_int c ^ "/" ^ string_of_int tot
  | Bytes (c, tot) -> Display_text.bytes c ^ " / " ^ Display_text.bytes tot

(* The pair the metric ends on sizes the cell before the first update; the pair
   it holds now is wider only where a unit changes under it ("1023.99 KiB"
   against a total of "1.00 MiB"). *)
let count_width metric =
  let full =
    match metric with
    | Spin -> Spin
    | Count (_, total) -> Count (total, total)
    | Bytes (_, total) -> Bytes (total, total)
  in
  max (String.length (count_text metric)) (String.length (count_text full))

type t = {
  ppf : Format.formatter;
  size : (unit -> int * int) option;
  depth : Color.depth option;
  now : unit -> float;
  header : string option;
  layout : Display_line.t;
  start : float;
  mutable width : int;
  mutable height : int;
  running : row Queue.t;
  mutable stale : int;
  mutable count_cell : int;
  by_key : (string, row) Hashtbl.t;
  mutable next_id : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable result : ([ `Ok of string | `Fail of string ] * string list) option;
  mutable scene : (height:int -> Format.formatter -> unit) option;
  mutable frame : string array;
  mutable frame_widths : int array;
  mutable painted_at : float;
  mutable changes : int;
  mutable painted : int * int * int * int;
  mutable closed : bool;
  mutable suspended : int;
  mutable partial : bool;
  mutable release : unit -> unit;
}

and row = {
  id : int;
  key : string option;
  disp : t;
  parent : row option;
  log_lines : int;
  logs : log Queue.t;
  mutable log_prev : float;
  mutable label : string;
  mutable metric : metric;
  transient : bool;
  started_at : float;
  mutable ended_at : float option;
  mutable message : string option;
  mutable outcome : outcome option;
  mutable children : row list;
  mutable child_limit : int;
  mutable last_cur : int64;
  mutable last_t : float;
  mutable rate : float;
}

let pp ppf t =
  Fmt.pf ppf "<display %dx%d %d/%d tasks%s>" t.width t.height t.finished
    t.started
    (if t.closed then " closed" else "")

(* On a wide terminal, a count or a timer at the right edge is far from
   its label. *)
let room t = min t.width 100
let is_live t = Option.is_some t.size
let is_running r = r.ended_at = None

let elapsed r =
  match r.ended_at with
  | Some f -> f -. r.started_at
  | None -> r.disp.now () -. r.started_at

let rec depth r = match r.parent with None -> 0 | Some p -> 1 + depth p

(* Finished rows stay in [running] until they are as many as the running
   ones, then all go at once: a row is taken out in amortised constant time,
   and the queue holds at most twice the rows that run. *)
let compact t =
  if t.stale > Queue.length t.running / 2 then begin
    let keep = Queue.create () in
    Queue.iter (fun r -> if is_running r then Queue.add r keep) t.running;
    Queue.clear t.running;
    Queue.transfer keep t.running;
    t.stale <- 0
  end

let running_rows t =
  Queue.fold (fun acc r -> if is_running r then r :: acc else acc) [] t.running
  |> List.rev

let running_children r = List.filter is_running r.children

(* For each running row, in order: whether it is the last running row under
   its parent, and the same for each of its ancestors, innermost first. Rows
   start after their parent, so one pass forward sees each parent first. *)
let lasts rows =
  let last = Hashtbl.create 16 in
  let parent_id r = match r.parent with None -> -1 | Some p -> p.id in
  List.iter
    (fun r ->
      if not (Hashtbl.mem last (parent_id r)) then
        Hashtbl.add last (parent_id r) r.id)
    (List.rev rows);
  let flags = Hashtbl.create 16 in
  List.map
    (fun r ->
      let own = Hashtbl.find last (parent_id r) = r.id in
      let above =
        match r.parent with
        | None -> []
        | Some p -> Option.value (Hashtbl.find_opt flags p.id) ~default:[]
      in
      let l = own :: above in
      Hashtbl.replace flags r.id l;
      (r, l))
    rows
