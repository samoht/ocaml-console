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

let task r = (r.id, labels [] r)

let write t kind =
  let event = Event.make ~time:(t.now () -. t.start) kind in
  Display_region.write t (Event.to_string event ^ "\n")

let started r =
  write r.disp (`Start (task r, Option.map (fun p -> p.id) r.parent))

let progress r =
  match r.metric with
  | Spin -> (* A count never goes back to a spinner. *) ()
  | Count (cur, total) ->
      write r.disp
        (`Progress (task r, Int64.of_int cur, Int64.of_int total, `Items))
  | Bytes (cur, total) -> write r.disp (`Progress (task r, cur, total, `Bytes))

let renamed r = write r.disp (`Label (task r, r.label))

let output t r line =
  let level = match line.kind with Output -> None | Logs l -> Some l in
  write t
    (`Output (Option.map task r, line.text, (level :> Event.level option)))

let ended r =
  write r.disp
    (`End
       ( task r,
         (Option.value r.outcome ~default:`Cancelled :> Event.status),
         elapsed r,
         r.message ))

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
      write t (`Result (outcome, text, detail))
