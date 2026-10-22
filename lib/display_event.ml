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

let started r =
  let parent = Option.map (fun p -> p.id) r.parent in
  write r.disp (Event.start ?parent ~id:r.id (labels [] r))

let progress r =
  let count cur total unit =
    write r.disp (Event.progress ~id:r.id ~count:cur ~total ~unit (labels [] r))
  in
  match r.metric with
  | Spin -> (* A count never goes back to a spinner. *) ()
  | Count (cur, total) -> count (Int64.of_int cur) (Int64.of_int total) `Items
  | Bytes (cur, total) -> count cur total `Bytes

let renamed r = write r.disp (Event.label ~id:r.id ~text:r.label (labels [] r))

let output t r line =
  let level =
    match line.kind with Output -> None | Logs l -> Some (l :> Event.level)
  in
  let id = Option.map (fun r -> r.id) r and task = Option.map (labels []) r in
  write t (Event.output ?id ?task ?level line.text)

let ended r =
  let status = (Option.value r.outcome ~default:`Cancelled :> Event.status) in
  write r.disp
    (Event.end' ?text:r.message ~id:r.id ~status ~elapsed:(elapsed r)
       (labels [] r))

let result t =
  match t.result with
  | None -> ()
  | Some (result, detail) ->
      let outcome, text =
        match result with Ok m -> (`Ok, m) | Error m -> (`Error, m)
      in
      let detail =
        match detail with
        | [] -> None
        | lines -> Some (String.concat "\n" lines)
      in
      write t (Event.result ?detail ~outcome text)
