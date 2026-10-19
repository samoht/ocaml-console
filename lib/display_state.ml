(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metric = Spin | Count of int * int | Bytes of int64 * int64

let count_text = function
  | Spin -> ""
  | Count (c, tot) -> Fmt.str "%d/%d" c tot
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
  dppf : Format.formatter;
  plain : bool;
  styling : bool;
  now : unit -> float;
  wait : float -> unit;
  dimensions : unit -> int * int;
  char_width : (Uchar.t -> int) option;
  header : string option;
  default_style : Display_line.t;
  theme : Theme.t;
  palette : Color.t list;
  dstart : float;
  mutable width : int;
  mutable height : int;
  auto_width : bool;
  auto_height : bool;
  mutable rows : row list;
  mutable count_cell : int;
  by_key : (string, row) Hashtbl.t;
  mutable events_rev : Display_event.t list;
  mutable next_task_id : int;
  mutable pending :
    ([ `Ok of string | `Fail of string ] * string option) option;
  mutable finished : int;
  mutable live_height : int;
  mutable last_lines : string list;
  mutable painted_at : float;
  mutable torn_down : bool;
  mutable owns_activity : bool;
  mutable suspended : int;
  mutable frame : int;
  mutable drawn : int;
  mutable frame_waiters : (int * (unit -> unit)) list;
  mutable scene : (frame:int -> width:int -> Span.t list) option;
}

and row_log = {
  ltime : float;
  lgap : float;
  llevel : Display_event.level;
  lmessage : string;
  lkept : bool;
}

and row = {
  rid : int;
  rkey : string option;
  disp : t;
  parent : row option;
  style : Display_line.t;
  log_lines : int;
  mutable logs : row_log list;
  mutable log_prev_t : float;
  mutable rlabel : string;
  mutable metric : metric;
  transient : bool;
  group : bool;
  mutable requested : (Display_event.outcome * string option) option;
  mutable swept : bool;
  mutable waiting : bool;
  mutable spilled : row_log list;
  rstart : float;
  mutable rfinish : float option;
  mutable rmessage : string option;
  mutable routcome : Display_event.outcome option;
  mutable last_cur : int64;
  mutable last_t : float;
  mutable rate : float;
}

let pp ppf t =
  Fmt.pf ppf "<display %dx%d %d/%d rows%s>" t.width t.height t.finished
    (List.length t.rows)
    (if t.torn_down then " torn-down" else "")

(* Every SGR the display adds passes through these, so one answer decides
   colour for the whole surface. They shadow the unconditional {!Ansi} forms on
   purpose: a call site that reaches past them is a second answer. *)
let code t sequence = if t.styling then sequence else ""
let styled t sequence s = if t.styling then Ansi.styled sequence s else s
let dimmed t s = if t.styling then Ansi.dimmed s else s
let string_width t = Width.string_width ?char_width:t.char_width
let truncate t = Width.truncate ?char_width:t.char_width
let shorten t = Width.shorten ?char_width:t.char_width
let fold t = Width.fold ?char_width:t.char_width
let pad_left t = Width.pad_left ?char_width:t.char_width

let rec task_path r =
  match r.parent with
  | None -> [ r.rlabel ]
  | Some parent -> task_path parent @ [ r.rlabel ]

let elapsed t row =
  match row.rfinish with
  | Some f -> f -. row.rstart
  | None -> t.now () -. row.rstart

let rec depth = function None -> 0 | Some r -> 1 + depth r.parent
let indent row = String.make (2 * depth row.parent) ' '

let siblings t row =
  List.filter
    (fun r ->
      match (r.parent, row.parent) with
      | Some a, Some b -> a == b
      | None, None -> true
      | _ -> false)
    t.rows

let is_last_child t row =
  match List.rev (siblings t row) with last :: _ -> last == row | [] -> true

let rec lasts_path t row =
  let own = is_last_child t row in
  match row.parent with None -> [ own ] | Some p -> lasts_path t p @ [ own ]

let nested_lasts t row =
  match lasts_path t row with _ :: rest -> rest | [] -> []

let guide_prefix t row =
  match row.parent with
  | None -> ""
  | Some _ ->
      "  " ^ Tree.prefix ~guide:(Theme.guide t.theme) (nested_lasts t row)

let continuation t row =
  let g = Theme.guide t.theme in
  "  "
  ^ String.concat ""
      (List.map
         (fun is_last -> if is_last then Guide.space g else Guide.pipe g)
         (nested_lasts t row))

let children r =
  List.filter
    (fun child ->
      match child.parent with Some parent -> parent == r | None -> false)
    r.disp.rows

let running_children r =
  List.filter (fun child -> child.rfinish = None) (children r)

let rec under_open_group r =
  match r.parent with
  | None -> false
  | Some p -> (p.group && p.rfinish = None) || under_open_group p

let waits_for_group r = (not r.disp.plain) && under_open_group r
