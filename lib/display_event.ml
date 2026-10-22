(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state
module Event = Progress.Event

let is_json t =
  (not (is_live t)) && match t.form with Json -> true | Plain | Quiet -> false

(* The labels from the top task down to [r]. *)
let rec labels acc r =
  let acc = r.label :: acc in
  match r.parent with None -> acc | Some p -> labels acc p

let write t event =
  let time = since_start t in
  Display_region.write t (Event.to_string (event ~time) ^ "\n")

let task r = (r.id, labels [] r)

let started r =
  let parent = Option.map (fun p -> p.id) r.parent in
  write r.disp (Event.start ?parent (task r))

let progress r =
  let count cur total unit =
    write r.disp (Event.progress ~count:cur ~total ~unit (task r))
  in
  match r.metric with
  | Spin -> (* A count never goes back to a spinner. *) ()
  | Count (cur, total) -> count (Int64.of_int cur) (Int64.of_int total) `Items
  | Bytes (cur, total) -> count cur total `Bytes

let renamed r = write r.disp (fun ~time -> Event.label ~time (task r) r.label)

let output t r line =
  let level =
    match line.kind with Output -> None | Logs l -> Some (l :> Event.level)
  in
  let task = Option.map task r in
  write t (Event.output ?task ?level line.text)

let ending = function
  | `Succeeded -> Event.succeeded
  | `Failed -> Event.failed
  | `Cancelled -> Event.cancelled

let ended r =
  let e = ending (Option.value r.outcome ~default:`Cancelled) in
  write r.disp (fun ~time ->
      e ~task:(task r) ?message:r.message ~time ~elapsed:(elapsed r) ())

(* The work ends as its result says, after the lines of its detail, and is
   cancelled without one. *)
let result t =
  let elapsed = since_start t in
  match t.result with
  | None -> write t (fun ~time -> Event.cancelled ~time ~elapsed ())
  | Some (result, detail) ->
      List.iter (fun l -> write t (Event.output l)) detail;
      let e, message =
        match result with
        | Ok m -> (Event.succeeded, m)
        | Error m -> (Event.failed, m)
      in
      write t (fun ~time -> e ~message ~time ~elapsed ())
