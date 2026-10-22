(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metric = Spin | Count of int * int | Bytes of int64 * int64
type outcome = [ `Succeeded | `Failed | `Cancelled ]
type level = [ `App | `Error | `Warning | `Info | `Debug ]
type kind = Output | Logs of level
type line = { gap : float; kind : kind; text : string; seq : int }
type form = Plain | Quiet | Json

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
  form : form;
  depth : Color.depth option;
  now : unit -> float;
  summary : string option;
  layout : Display_layout.t;
  keep_ended : bool;
  start : float;
  mutable width : int;
  mutable height : int;
  running : row Queue.t;
  mutable stale : int;
  mutable count_cell : int;
  by_key : (string, row) Hashtbl.t;
  mutable next_id : int;
  mutable next_line : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable result : ((string, string) result * string list) option;
  mutable view : (t -> width:int -> height:int -> Span_core.t) option;
  mutable frame : string array;
  mutable frame_widths : int array;
  mutable painted_at : float;
  mutable changes : int;
  mutable painted : int * int * int * int;
  mutable closed : bool;
  mutable closing : bool;
  mutable suspended : int;
  mutable partial : bool;
  writes : string Queue.t;
  mutable writing : bool;
  mutable release : unit -> unit;
  mutable ending : row -> unit;
  mutable root : row option;
  scope : scope;
}

and scope = {
  current : unit -> row option;
  within : row -> (unit -> unit) -> unit;
  cancelled : exn -> bool;
  write_error : exn -> bool;
}

and row = {
  id : int;
  key : string option;
  disp : t;
  parent : row option;
  log_lines : int;
  window : line Queue.t;
  output : line Queue.t;
  mutable output_bytes : int;
  mutable dropped : int;
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
  mutable gone : bool;
  mutable held : int;
  mutable weight : int;
  mutable shed : int;
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
let write_error t = function Sys_error _ -> true | e -> t.scope.write_error e
let is_live t = Option.is_some t.size

let is_plain t =
  (not (is_live t)) && match t.form with Plain -> true | Quiet | Json -> false

let keeps_output t =
  is_live t || match t.form with Quiet -> true | Plain | Json -> false

let is_running r = r.ended_at = None
let is_root r = r.id < 0

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
let children r = List.rev (List.filter (fun c -> not c.gone) r.children)

(* A display has its root from the time it is made. *)
let root t = Option.get t.root
