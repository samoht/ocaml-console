(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The rows of a display on an emulated terminal: their columns, as tasks
   start, progress and end. *)

open Alcobar
open Console

type pack =
  | Begin of int option * int
  | Items of int * int * int
  | Bytes_of of int * int
  | Finish of int * int
  | Tick

let pack =
  choose
    [
      map [ option uint8; uint8 ] (fun p n -> Begin (p, n));
      map [ uint8; uint8; uint8 ] (fun i c n -> Items (i, c, n));
      map [ uint8; uint8 ] (fun i c -> Bytes_of (i, c));
      map [ uint8; uint8 ] (fun i o -> Finish (i, o));
      const Tick;
    ]

let pp_pack ppf = function
  | Begin (p, n) -> Fmt.pf ppf "begin %a %d" Fmt.(option int) p n
  | Items (i, c, n) -> Fmt.pf ppf "items %d %d %d" i c n
  | Bytes_of (i, c) -> Fmt.pf ppf "bytes %d %d" i c
  | Finish (i, o) -> Fmt.pf ppf "finish %d %d" i o
  | Tick -> Fmt.string ppf "tick"

let pack = with_printer pp_pack pack

(* [start_cell s row] is the cell where [s] starts in [row], counting each
   glyph as one cell. *)
let start_cell s row =
  let s = Display_harness.glyphs s in
  let n = Array.length s in
  let rec go i =
    if i + n > Array.length row then None
    else if Array.for_all2 Uchar.equal s (Array.sub row i n) then Some i
    else go (i + 1)
  in
  go 0

type laid = {
  ends : int;  (** The cell after the label. *)
  bar : (int * int) option;  (** The first and last cells of the bar. *)
  last : int;  (** The cell after the last glyph. *)
}

let laid_out label row =
  let cells = Display_harness.glyphs row in
  Option.map
    (fun at ->
      {
        ends = at + String.length label;
        bar = Display_harness.bar_span row;
        last = Array.length cells;
      })
    (start_cell label cells)

(* Besides a wider label, two things widen the columns the rows share: a
   task that counts bytes, and an item total with more digits. *)
let widening tasks =
  List.fold_left
    (fun (bytes, items) (task, _) ->
      match Task.progress task with
      | `Bytes _ -> (true, items)
      | `Items (_, n) -> (bytes, max items (String.length (string_of_int n)))
      | `Spin -> (bytes, items))
    (false, 0) tasks

(* [apply_pack ~root ~now ~tasks step] applies [step] to [tasks] under
   [root] and to the clock [now]. [tasks] pairs each task with its label. *)
let apply_pack ~root ~now ~tasks step =
  let pick i =
    match !tasks with
    | [] -> None
    | l -> Some (fst (List.nth l (i mod List.length l)))
  in
  match step with
  | Begin (p, n) when List.length !tasks < 12 ->
      let label =
        Fmt.str "t%02d%s" (List.length !tasks) (String.make (n mod 24) '-')
      in
      let parent = Option.value (Option.bind p pick) ~default:root in
      tasks := !tasks @ [ (Task.start parent label, label) ]
  | Begin _ -> ()
  | Items (i, c, n) ->
      let total = [| 5; 12; 120; 1200; 99_999 |].(n mod 5) in
      Option.iter
        (fun task ->
          try Task.set_items task ~count:(c mod (total + 1)) ~total
          with Invalid_argument _ -> ())
        (pick i)
  | Bytes_of (i, c) ->
      let total = 2147483648L in
      let count = Int64.(div (mul total (of_int c)) 255L) in
      Option.iter
        (fun task ->
          try Task.set_bytes task ~count ~total with Invalid_argument _ -> ())
        (pick i)
  | Finish (i, o) ->
      Option.iter (fun task -> Display_harness.finish task o) (pick i)
  | Tick -> now := !now +. 0.25

(* [run_packs steps w ~frame] runs [steps] on a region. After each step it
   calls [frame rows tasks] with the rows on the screen and the tasks with
   their labels. *)
let run_packs steps w ~frame =
  let width = 40 + (w mod 81) and height = 24 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let ppf = Display_harness.feeding vte in
  Theme.set ppf Theme.dos;
  let now = ref 0. in
  let root = Task.root ~label:"HEADER" ~now:(fun () -> !now) () in
  let d =
    Progress.show ~keep_ended:true ~size:(fun () -> (width, height)) ppf root
  in
  let tasks = ref [] in
  List.iter
    (fun step ->
      apply_pack ~root ~now ~tasks step;
      Display.redraw d;
      frame (List.filter (( <> ) "") (Console_vte.screen vte)) !tasks)
    steps;
  Display.close d

let row_showing rows label =
  List.find_opt (fun r -> Display_harness.contains (label ^ " ") r) rows

let check_rows_pack ~previous rows tasks =
  let shown = String.concat "\n" rows in
  let laid =
    List.filter_map
      (fun (_, label) -> Option.bind (row_showing rows label) (laid_out label))
      tasks
  in
  match List.filter_map (fun l -> l.bar) laid with
  | [] -> ()
  | (first, last) :: _ as bars ->
      if List.exists (( <> ) (first, last)) bars then
        failf "the bars are not in one column:@.%s" shown;
      let column = List.fold_left (fun n l -> max n l.ends) 0 laid in
      if first <> column + 1 then
        failf "the bars start at %d, after the labels at %d:@.%s" first column
          shown;
      let ends = List.sort_uniq Int.compare (List.map (fun l -> l.last) laid) in
      (match ends with
      | [ _ ] -> ()
      | _ -> failf "the rows end in several columns:@.%s" shown);
      let key = (column, widening tasks) in
      (match !previous with
      | Some (_, (first', _), _, shown') when first < first' ->
          failf "the bars moved left:@.%s@.then@.%s" shown' shown
      | Some (key', bar', ends', shown')
        when key = key' && (bar' <> (first, last) || ends' <> ends) ->
          failf "the columns moved:@.%s@.then@.%s" shown' shown
      | _ -> ());
      previous := Some (key, (first, last), ends, shown)

(* The rows of a region pack from the left. The bars of all rows start at
   the same column, one cell after the widest label, and have the same
   width. Every row with columns ends at the same column. During a run at
   one width, the bars never move left, and the columns move only when they
   widen. *)
let test_rows_pack steps w =
  let previous = ref None in
  run_packs steps w ~frame:(check_rows_pack ~previous)

(* [from_label label row] is [row] from the cell where [label] starts. *)
let from_label label row =
  let cells = Display_harness.glyphs row in
  Option.map
    (fun at ->
      let b = Buffer.create 80 in
      for i = at to Array.length cells - 1 do
        Buffer.add_utf_8_uchar b cells.(i)
      done;
      Buffer.contents b)
    (start_cell label cells)

(* [label_column rows tasks] is the cell after the widest label on the
   screen. *)
let label_column rows tasks =
  List.fold_left
    (fun n (_, label) ->
      match row_showing rows label with
      | None -> n
      | Some r ->
          Option.fold ~none:n ~some:(fun l -> max n l.ends) (laid_out label r))
    0 tasks

let check_ended_rows ~seen rows tasks =
  let key = (label_column rows tasks, widening tasks) in
  List.iter
    (fun (task, label) ->
      if Task.state task <> `Running then
        match Option.bind (row_showing rows label) (from_label label) with
        | None -> ()
        | Some row -> (
            match Hashtbl.find_opt seen label with
            | Some (key', row') when key = key' && row <> row' ->
                failf "the row of %s moved:@.%s@.then@.%s@.in@.%s" label row'
                  row (String.concat "\n" rows)
            | _ -> Hashtbl.replace seen label (key, row)))
    tasks

(* The row of an ended task, from its label on, does not change while the
   columns do not change. This holds as time passes and as other tasks
   progress, start and end. *)
let test_ended_rows_stay steps w =
  let seen = Hashtbl.create 16 in
  run_packs steps w ~frame:(check_ended_rows ~seen)

let suite =
  ( "progress_row",
    [
      test_case "rows pack from the left, their bars in one column"
        [ list pack; uint8 ]
        test_rows_pack;
      test_case "the row of a task that ended stays as it is"
        [ list pack; uint8 ]
        test_ended_rows_stay;
    ] )
