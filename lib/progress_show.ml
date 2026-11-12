(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Layout = Progress_layout
module V = View_core
open Task_core

let tasks _ = V.tasks
let lines vs = V.concat ~sep:(V.text "\n") vs

let row ?(layout = Layout.default) r =
  V.deferred ~kind:(Layout.reads layout) (fun c ->
      let look = Ctx.look c and width = Ctx.width c in
      if Progress_tree.draws_label layout r then widen_label r;
      let columns =
        match Layout.label_width layout with
        | Some w ->
            {
              (Progress_row.columns look r) with
              label = w;
              least = min w Progress_row.narrowest;
              fixed = true;
            }
        | None -> Progress_row.columns look r
      in
      if is_running r then
        Progress_row.live look ~time:(Ctx.time c) ?width ~columns layout r
      else lines (Progress_row.ended look ?width ~columns ~whole:false r))

let tree ?(layout = Layout.default) r = Progress_tree.view ~layout r

let summary root =
  V.deferred ~kind:tasks (fun c ->
      Option.value ~default:V.empty
        (Progress_row.summary (Ctx.look c) ?width:(Ctx.width c) (root_of root)))

let verdict root =
  V.deferred ~kind:tasks (fun c ->
      lines
        (Progress_row.verdict (Ctx.look c) ?width:(Ctx.width c) (root_of root)))

(* On a wide terminal, a count or a timer at the right edge is far from its
   label. So the rows use at most 100 columns. *)
let view ?(layout = Layout.default) root =
  let root = root_of root in
  V.deferred ~kind:(Layout.reads layout) (fun c ->
      let width = Option.map (min 100) (Ctx.width c) in
      let look = Ctx.look c in
      let below =
        Option.to_list (Progress_row.summary look ?width root)
        @ Progress_row.verdict look ?width root
      in
      let height = Option.map (fun h -> h - List.length below) (Ctx.height c) in
      let tree =
        match height with
        | Some h when h <= 0 -> V.empty
        | _ -> V.resolve (Ctx.with_room ~width ~height c) (tree ~layout root)
      in
      V.vcat (tree :: below))

type form = Live | Plain | Quiet | Json

(* The size of the region the rows are redrawn in, [None] when lines are
   printed instead, and the form of the display. *)
let shape ?size = function
  | `Auto -> (size, if Option.is_some size then Live else Plain)
  | `Tty -> (Some (Option.value size ~default:(fun () -> (80, 24))), Live)
  | `Plain -> (None, Plain)
  | `Quiet -> (None, Quiet)
  | `Json -> (None, Json)

let print d vs = Display_core.print_lines d vs

(* A top task that ended waits in the region with its subtasks. It moves to
   the scrollback, with the other waiting trees, when something else is
   printed there, when the region has no room left for them, or when the
   display closes. So rows printed one after another share their columns.
   The trees move in the order the region draws them: the order they
   started, the first at the top. A tree that holds a failure or a
   cancellation with output moves at once, after the waiting trees, so the
   output shows. A tree that draws no line moves at once. A task started
   under a waiting top task ends at once and joins its tree. *)

(* A waiting tree, and what [commit] made of it when it ended, while it still
   had its output. *)
type waiting = { task : Task_core.t; mutable view : V.t option }
type held = { mutable queue : waiting list; mutable moving : bool }

let held () = { queue = []; moving = false }

(* [w] inserted into [ws], which is in the order the tasks started. *)
let rec insert w = function
  | x :: rest when id x.task < id w.task -> x :: insert w rest
  | ws -> w :: ws

(* The highest task above [r], other than the root, that is still drawn. *)
let rec top_of r =
  match parent r with
  | Some p when (not (is_root p)) && not (gone p) -> top_of p
  | _ -> r

(* Whether [r], or a task under it, failed or was cancelled and kept some
   output. *)
let rec has_output r =
  outcome r <> Some `Succeeded
  && ((not (Queue.is_empty (output r))) || dropped r > 0)
  || List.exists has_output (children r)

(* [moved d w] moves the tree [w] from the region to the scrollback, as
   [commit] drew it. The tree is laid out first, then forgotten, then
   printed, so it never shows twice. Its label column is kept, so the rows
   printed after it line up with it. If the layout raises, the task is still
   forgotten and nothing is printed. *)
let moved d w =
  let look = Display_core.look d in
  let width = min (Display_core.width d) 100 in
  let committed =
    protect
      (fun () ->
        Display_core.caught d ~default:None (fun () ->
            Option.map (V.resolve (Ctx.v ~width look)) w.view))
      ~finally:(fun () -> forget w.task)
  in
  match committed with
  | Some v when not (V.is_empty v) -> Display_core.print d v
  | _ -> Display_core.touch d

(* [move h d ~layout ~until] moves the trees that [h] holds, in the order
   they started, until [until ()] is true. They all share one label column.
   Printing a tree does not move the next tree from inside that print. *)
let move h d ~layout ~until =
  if (not h.moving) && h.queue <> [] && not (until ()) then begin
    h.moving <- true;
    protect
      (fun () ->
        List.iter (fun w -> Progress_tree.measure ~layout w.task) h.queue;
        let rec go () =
          match h.queue with
          | w :: rest when not (until ()) ->
              h.queue <- rest;
              if not (gone w.task) then moved d w;
              go ()
          | _ -> ()
        in
        go ())
      ~finally:(fun () -> h.moving <- false)
  end

let move_all h d ~layout = move h d ~layout ~until:(fun () -> false)

(* The region keeps one line for the summary and one line that the display
   leaves free. *)
let fits d root =
  let below = if Option.is_some (Work.summary (work root)) then 1 else 0 in
  Progress_tree.lines root + below <= Display_core.height d - 1

let hold h d ~layout ~commit r =
  let top = top_of r in
  if not (is_running top) then begin
    let view =
      Display_core.caught d ~default:None (fun () ->
          match commit with None -> Some (tree ~layout top) | Some f -> f top)
    in
    match List.find_opt (fun w -> w.task == top) h.queue with
    | Some w -> w.view <- view
    | None ->
        let w = { task = top; view } in
        if Progress_tree.lines top = 0 then moved d w
        else begin
          h.queue <- insert w h.queue;
          if has_output top then move_all h d ~layout
        end
  end

(* Only a task that starts or ends changes the number of lines the region
   needs. So only then do the waiting trees move to make room. *)
let on_live d ~layout ~keep_ended ~commit ~held:h root = function
  | Started _ ->
      Display_core.touch d;
      move h d ~layout ~until:(fun () -> fits d root)
  | Progressed _ | Renamed _ -> Display_core.touch d
  | Line (r, line) ->
      let look = Display_core.look d and width = Display_core.width d in
      if is_root r then print d (Progress_row.message look ~width line)
      else if is_running r then Display_core.touch d
      else print d (Progress_row.labelled look ~width:(min width 100) r line)
  | Ended (r, _) ->
      if (not (is_root r)) && not keep_ended then hold h d ~layout ~commit r;
      Display_core.touch d;
      move h d ~layout ~until:(fun () -> fits d root)

let on_plain d ~keep_ended = function
  | Started r ->
      if not (transient r) then
        print d [ Progress_row.started (Display_core.look d) r ]
  | Progressed _ | Renamed _ -> ()
  | Line (r, line) ->
      let look = Display_core.look d in
      if is_root r then print d (Progress_row.message look line)
      else print d (Progress_row.labelled look r line)
  | Ended (r, e) ->
      if not (is_root r) then begin
        if not (transient r && e.outcome = `Succeeded) then
          print d [ Progress_row.appended (Display_core.look d) r e ];
        if not keep_ended then forget r
      end

(* With [plain], the program prints its own lines for the changes of tasks,
   in place of the lines console prints. The lines of the root print as they
   do without [plain]. *)
let on_own d ~keep_ended root plain = function
  | (Line (r, _) | Ended (r, _)) as change when is_root r ->
      on_plain d ~keep_ended change
  | change ->
      protect
        (fun () ->
          match event change ~time:(since_start (work root)) with
          | Some e ->
              Display_core.caught d ~default:() (fun () ->
                  Option.iter (Display_core.print d) (plain e))
          | None -> ())
        ~finally:(fun () ->
          match change with
          | Ended (r, _) when not keep_ended -> forget r
          | _ -> ())

(* The output a task kept, under its line. *)
let kept look r =
  let all = Queue.fold (fun acc l -> l :: acc) [] (output r) in
  (if dropped r > 0 then [ Progress_row.dropped look (dropped r) ] else [])
  @ List.concat_map (Progress_row.under look) (List.rev all)

let on_quiet d = function
  | Started _ | Progressed _ | Renamed _ -> ()
  | Line (r, line) ->
      let look = Display_core.look d in
      if is_root r then print d (Progress_row.message look line)
      else if not (is_running r) then
        print d (Progress_row.labelled look r line)
  | Ended (r, e) ->
      if not (is_root r) then begin
        (match e.outcome with
        | `Failed | `Cancelled ->
            let look = Display_core.look d in
            print d (Progress_row.appended look r e :: kept look r)
        | `Succeeded -> ());
        forget r
      end

let on_json d root change =
  (match event change ~time:(since_start (work root)) with
  | Some e -> Display_core.write d (Event.to_string e ^ "\n")
  | None -> ());
  match change with Ended (r, _) when not (is_root r) -> forget r | _ -> ()

(* The default view of a root in each form: the tree and the summary for
   [Live] and [Plain], only the final message of the work for [Quiet], and
   nothing for [Json]. *)
let view_of_form ~layout root = function
  | Live | Plain -> view ~layout root
  | Quiet -> verdict root
  | Json -> V.empty

let show_with ?write_error ?cancelled ?atomic ?(progress = `Auto)
    ?(layout = Layout.default) ?view ?(keep_ended = false) ?commit ?plain ?delay
    ?size ppf root =
  let root = root_of root in
  let shown_size, form = shape ?size progress in
  let default = view_of_form ~layout root form in
  let shown =
    match form with
    | Live | Plain -> Option.value view ~default
    | Quiet | Json -> default
  in
  let once_width =
    match (form, size) with
    | Plain, Some given -> Some (fun () -> fst (given ()))
    | _ -> None
  in
  let d =
    Display_core.v ?size:shown_size ?once_width ?write_error ?cancelled ?atomic
      ?delay ~fallback:default
      ~now:(Work.now (work root))
      ppf shown
  in
  Display_core.told d;
  Display_core.on_close d (fun () -> if is_running root then cancel root);
  holding (work root) (fun () -> Display_core.drawing d);
  Display_core.on_settle d (fun () -> settle root);
  let watching on_change =
    watch (work root) (fun change -> Display_core.reporting d on_change change)
  in
  (match form with
  | Live ->
      let h = held () in
      Display_core.on_print d (fun () -> move_all h d ~layout);
      watching (on_live d ~layout ~keep_ended ~commit ~held:h root)
  | Plain -> (
      match plain with
      | None -> watching (on_plain d ~keep_ended)
      | Some plain -> watching (on_own d ~keep_ended root plain))
  | Quiet -> watching (on_quiet d)
  | Json ->
      Display_core.on_output d (fun s -> log root (Console_text.strip s));
      watching (on_json d root));
  d
