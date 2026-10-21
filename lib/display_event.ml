(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let is_json t =
  (not (is_live t)) && match t.form with Json -> true | Plain | Quiet -> false

type value = String of string | Int of int | Int64 of int64 | Number of float

(* The labels from the top task down to [r]. *)
let rec labels acc r =
  let acc = r.label :: acc in
  match r.parent with None -> acc | Some p -> labels acc p

(* One object on a line: the time and the event, the task's id and labels
   if the event is a task's, the labels of none for output of no task, then
   [fields] in order. *)
let write t subject event fields =
  let buf = Buffer.create 128 in
  let name n =
    Buffer.add_string buf ",\"";
    Buffer.add_string buf n;
    Buffer.add_string buf "\":"
  in
  let field (n, value) =
    name n;
    match value with
    | String s -> Display_json.add_string buf s
    | Int n -> Buffer.add_string buf (string_of_int n)
    | Int64 n -> Buffer.add_string buf (Int64.to_string n)
    | Number x -> Display_json.add_number buf x
  in
  let task path =
    name "task";
    Buffer.add_char buf '[';
    List.iteri
      (fun i label ->
        if i > 0 then Buffer.add_char buf ',';
        Display_json.add_string buf label)
      path;
    Buffer.add_char buf ']'
  in
  Buffer.add_string buf "{\"time\":";
  Display_json.add_number buf (t.now () -. t.start);
  field ("event", String event);
  (match subject with
  | `Task r ->
      field ("id", Int r.id);
      task (labels [] r)
  | `No_task -> task []
  | `Display -> ());
  List.iter field fields;
  Buffer.add_string buf "}\n";
  Display_region.write t (Buffer.contents buf)

let parent r =
  match r.parent with Some p -> [ ("parent", Int p.id) ] | None -> []

let started r = write r.disp (`Task r) "start" (parent r)

let progress r =
  let fields =
    match r.metric with
    | Spin -> []
    | Count (cur, total) ->
        [ ("count", Int cur); ("total", Int total); ("unit", String "items") ]
    | Bytes (cur, total) ->
        [
          ("count", Int64 cur); ("total", Int64 total); ("unit", String "bytes");
        ]
  in
  write r.disp (`Task r) "progress" fields

let renamed r = write r.disp (`Task r) "label" [ ("text", String r.label) ]

let level = function
  | `App -> "app"
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Debug -> "debug"

let output t r line =
  let level =
    match line.kind with
    | Output -> []
    | Logs l -> [ ("level", String (level l)) ]
  in
  let subject = match r with Some r -> `Task r | None -> `No_task in
  write t subject "output" (("text", String line.text) :: level)

let status = function
  | `Succeeded -> "succeeded"
  | `Failed -> "failed"
  | `Cancelled -> "cancelled"

let ended r =
  let message =
    match r.message with Some m -> [ ("text", String m) ] | None -> []
  in
  write r.disp (`Task r) "end"
    (("status", String (status (Option.value r.outcome ~default:`Cancelled)))
    :: ("elapsed", Number (elapsed r))
    :: message)

let result t =
  match t.result with
  | None -> ()
  | Some (result, detail) ->
      let status, text =
        match result with Ok m -> ("ok", m) | Error m -> ("error", m)
      in
      let detail =
        match detail with
        | [] -> []
        | lines -> [ ("detail", String (String.concat "\n" lines)) ]
      in
      write t `Display "result"
        (("status", String status) :: ("text", String text) :: detail)
