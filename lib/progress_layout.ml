(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind =
  | Spinner
  | Mark
  | Label of Style.t
  | Bar of int option
  | Percent
  | Count
  | Rate
  | Eta
  | Elapsed
  | Spacer
  | Const of View_core.t
  | Custom of (Task_core.t -> View_core.t)
  | Choose of (Task_core.t -> piece list)

and piece = { kind : kind; stage : int option; mutable blank : bool option }

type t = piece list

let pieces t = t

let rec reads_in look k = function
  | [] -> k
  | { kind = Custom _ | Choose _; _ } :: rest ->
      reads_in look (View_core.join k View_core.moving) rest
  | { kind = Const v; _ } :: rest ->
      reads_in look (View_core.join k (View_core.kind v look)) rest
  | _ :: rest -> reads_in look k rest

(* A frame asks each view what it reads. A layout whose pieces read only the
   tasks, as the default one does, answers without walking its pieces. *)
let reads t =
  let own p =
    match p.kind with
    | Custom _ | Choose _ -> true
    | Const v -> not (View_core.is_flat v)
    | _ -> false
  in
  if List.exists own t then fun look -> reads_in look View_core.tasks t
  else fun _ -> View_core.tasks

(* Programs build their layouts as they load, before they show anything, so a
   constant piece is drawn to find whether it is blank only when a row first
   asks. A copy made before that asks again, and two domains that ask at once
   find the same answer. *)
let blank p =
  match (p.blank, p.kind) with
  | Some b, _ -> b
  | None, Const v ->
      let b = String.trim (View_core.to_string v) = "" in
      p.blank <- Some b;
      b
  | None, _ -> false

let one ?stage kind = [ { kind; stage; blank = None } ]

(* The stages at which pieces give way: 1 for the rate and the time left, 2
   for the bar and the percentage, 5 for the elapsed time. The label is cut at
   stage 3 and the count gets shorter at stage 4, as [Progress_row] draws
   them. *)
let spinner = one Spinner
let label ?(style = Style.bold) () = one (Label style)

let bar ?width () =
  (match width with
  | Some n when n < 0 ->
      invalid_arg "Console.Progress.Layout.bar: negative width"
  | _ -> ());
  one ~stage:2 (Bar width)

let percent = one ~stage:2 Percent
let count = one Count
let rate = one ~stage:1 Rate
let eta = one ~stage:1 Eta
let elapsed = one ~stage:5 Elapsed
let spacer = one Spacer
let const v = one (Const v)
let custom f = one (Custom f)
let choose f = one (Choose f)

let give_way n t =
  if n < 1 then invalid_arg "Console.Progress.Layout.give_way: stage below 1";
  List.map (fun p -> { p with stage = Some n }) t

let is_label p = match p.kind with Label _ -> true | _ -> false
let has_label pieces = List.exists is_label pieces
let is_spacer p = match p.kind with Spacer -> true | _ -> false

(* A row has at most one label, the piece that is cut, and one spacer, the
   piece that takes the room left. *)
let checked t =
  let count is = List.length (List.filter is t) in
  if count is_label > 1 then
    invalid_arg "Console.Progress.Layout: more than one label";
  if count is_spacer > 1 then
    invalid_arg "Console.Progress.Layout: more than one spacer";
  t

let ( ++ ) a b = checked (a @ b)

(* The pieces that a [Choose] gives take its stage, if it has one. *)
let rec for_task t r =
  checked
    (List.concat_map
       (fun p ->
         match p.kind with
         | Choose f ->
             let chosen = for_task (f r) r in
             if p.stage = None then chosen
             else List.map (fun q -> { q with stage = p.stage }) chosen
         | _ -> [ p ])
       t)

let list ?sep ts =
  let rec go = function
    | [] -> []
    | [ t ] -> t
    | t :: rest -> t @ Option.value sep ~default:[] @ go rest
  in
  checked (go ts)

let space = const (View_core.text " ")

let packed lead =
  list
    [
      lead;
      space;
      label ();
      space;
      bar ();
      space;
      count;
      space;
      rate;
      space;
      elapsed;
    ]

let default = packed spinner
let ended = packed (one Mark)

let pp_kind ppf = function
  | Spinner -> Fmt.string ppf "spinner"
  | Mark -> Fmt.string ppf "mark"
  | Label _ -> Fmt.string ppf "label"
  | Bar None -> Fmt.string ppf "bar"
  | Bar (Some n) -> Fmt.pf ppf "bar %d" n
  | Percent -> Fmt.string ppf "percent"
  | Count -> Fmt.string ppf "count"
  | Rate -> Fmt.string ppf "rate"
  | Eta -> Fmt.string ppf "eta"
  | Elapsed -> Fmt.string ppf "elapsed"
  | Spacer -> Fmt.string ppf "spacer"
  | Const v -> Fmt.pf ppf "const %S" (View_core.to_string v)
  | Custom _ -> Fmt.string ppf "custom"
  | Choose _ -> Fmt.string ppf "choose"

let pp = Fmt.Dump.list (fun ppf p -> pp_kind ppf p.kind)
