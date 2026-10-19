(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]

type item = {
  id : int;
  key : string option;
  parent : int option;
  label : string;
  progress : progress;
  elapsed : float;
  logs : Display_event.t list;
}

let id t = t.id
let key t = t.key
let parent t = t.parent
let label t = t.label
let progress t = t.progress
let elapsed t = t.elapsed
let logs t = t.logs

type t = item list

let items t = t

let pp_item ppf item =
  let pp_progress ppf = function
    | `Spin -> Fmt.string ppf "running"
    | `Count (cur, total) -> Fmt.pf ppf "%d/%d" cur total
    | `Bytes (cur, total) ->
        Fmt.pf ppf "%s/%s" (Display_text.bytes cur) (Display_text.bytes total)
  in
  Fmt.pf ppf "@[%d%s %s (%a, %s)@]" item.id
    (match item.parent with None -> "" | Some parent -> Fmt.str "<-%d" parent)
    item.label pp_progress item.progress
    (Display_text.span item.elapsed)

let pp = Fmt.Dump.list pp_item

let item t (r : Display_state.row) =
  let progress : progress =
    match r.metric with
    | Spin -> `Spin
    | Count (cur, total) -> `Count (cur, total)
    | Bytes (cur, total) -> `Bytes (cur, total)
  in
  {
    id = r.rid;
    key = r.rkey;
    parent =
      Option.map (fun (parent : Display_state.row) -> parent.rid) r.parent;
    label = r.rlabel;
    progress;
    elapsed = Display_state.elapsed t r;
    logs =
      List.map
        (fun (log : Display_state.row_log) : Display_event.t ->
          {
            elapsed = log.ltime;
            task = Some r.rid;
            key = r.rkey;
            path = Display_state.task_path r;
            kind = `Log log.llevel;
            message = log.lmessage;
            detail = None;
          })
        r.logs;
  }

let of_state (t : Display_state.t) =
  List.filter_map
    (fun (r : Display_state.row) ->
      if r.rfinish <> None then None else Some (item t r))
    t.rows
