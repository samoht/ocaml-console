(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metric = Spin | Count of int * int | Bytes of int64 * int64
type outcome = [ `Succeeded | `Failed | `Cancelled ]
type level = [ `App | `Error | `Warning | `Info | `Debug ]
type kind = Output | Logs of level
type line = { gap : float; kind : kind; text : string; seq : int }
type memo = ..

type scope = {
  current : unit -> t option;
  within : t -> (unit -> unit) -> unit;
  cancelled : exn -> bool;
}

and change =
  | Started of t
  | Progressed of t
  | Renamed of t
  | Line of t * line
  | Ended of t

and work = {
  now : unit -> float;
  start : float;
  summary : string option;
  scope : scope;
  by_key : (string, t) Hashtbl.t;
  mutable next_id : int;
  mutable next_line : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable count_cell : int;
  mutable short_cell : int;
  mutable rate_cell : int;
  mutable kept_cells : int array;
  mutable running_cells : (int, int) Hashtbl.t array;
  mutable watchers : (change -> unit) list;
  mutable ending : t -> unit;
  mutable changes : int;
}

and t = {
  id : int;
  key : string option;
  work : work;
  parent : t option;
  log_lines : int;
  window : line Queue.t;
  output : line Queue.t;
  mutable output_bytes : int;
  mutable dropped : int;
  mutable log_prev : float;
  mutable label : string;
  mutable label_cells : int;
  mutable metric : metric;
  transient : bool;
  started_at : float;
  mutable ended_at : float option;
  mutable message : string option;
  mutable outcome : outcome option;
  mutable finishing : bool;
  mutable children : t list;
  mutable child_limit : int;
  mutable gone : bool;
  mutable held : int;
  mutable weight : int;
  mutable shed : int;
  mutable last_cur : float;
  mutable last_t : float;
  mutable rate : float;
  mutable version : int;
  mutable memo : memo list;
}

let is_root r = r.id < 0
let is_running r = r.ended_at = None

let elapsed r =
  match r.ended_at with
  | Some f -> f -. r.started_at
  | None -> r.work.now () -. r.started_at

let since_start w = w.now () -. w.start
let rec depth r = match r.parent with None -> 0 | Some p -> 1 + depth p
let children r = List.rev (List.filter (fun c -> not c.gone) r.children)
let running_children r = List.filter is_running r.children

let path r =
  let rec up acc r =
    if is_root r then acc
    else
      match r.parent with
      | None -> r.label :: acc
      | Some p -> up (r.label :: acc) p
  in
  up [] r

let rec root_of r = match r.parent with None -> r | Some p -> root_of p
let changed r = r.version <- r.version + 1
let keep r m = r.memo <- m

let rec is_async = function
  | Out_of_memory | Stack_overflow | Sys.Break -> true
  | Fun.Finally_raised e -> is_async e
  | _ -> false

let protect ~finally f =
  match f () with
  | v ->
      finally ();
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try finally () with later when is_async e || not (is_async later) -> ());
      Printexc.raise_with_backtrace e bt

let rec run_all = function
  | [] -> ()
  | f :: rest -> protect f ~finally:(fun () -> run_all rest)

(* [tell change ws] gives [change] to every watcher of [ws], whatever one of
   them raises, as {!run_all} does, but allocates nothing when none raises:
   every progress update makes a change. *)
let rec tell change = function
  | [] -> ()
  | f :: rest -> (
      match f change with
      | () -> tell change rest
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          (try tell change rest
           with later when is_async e || not (is_async later) -> ());
          Printexc.raise_with_backtrace e bt)

let notify w change =
  w.changes <- w.changes + 1;
  (match change with
  | Started r | Progressed r | Renamed r | Line (r, _) | Ended r -> changed r);
  tell change w.watchers

let watch w f = w.watchers <- w.watchers @ [ f ]
let on_ending r f = r.work.ending <- f

let local () =
  let stack = ref [] in
  {
    current = (fun () -> match !stack with r :: _ -> Some r | [] -> None);
    within =
      (fun r f ->
        let saved = !stack in
        stack := r :: saved;
        Fun.protect f ~finally:(fun () -> stack := saved));
    cancelled = (fun _ -> false);
  }

let row w ~id ?key ?parent ~log_lines ~transient label =
  let now = w.now () in
  {
    id;
    key;
    work = w;
    parent;
    log_lines;
    window = Queue.create ();
    output = Queue.create ();
    output_bytes = 0;
    dropped = 0;
    log_prev = now;
    label = Task_text.label label;
    label_cells = -1;
    metric = Spin;
    transient;
    started_at = now;
    ended_at = None;
    message = None;
    outcome = None;
    finishing = false;
    children = [];
    child_limit = 16;
    gone = false;
    held = 0;
    weight = 0;
    shed = 0;
    last_cur = 0.;
    last_t = now;
    rate = 0.;
    version = 0;
    memo = [];
  }

let root ?label ?scope ~now () =
  let start = now () in
  let w =
    {
      now;
      start;
      summary = Option.map Task_text.label label;
      scope = Option.value scope ~default:(local ());
      by_key = Hashtbl.create 16;
      next_id = 0;
      next_line = 0;
      started = 0;
      finished = 0;
      failed = 0;
      cancelled = 0;
      count_cell = 0;
      short_cell = 0;
      rate_cell = 0;
      kept_cells = [||];
      running_cells = [||];
      watchers = [];
      ending = ignore;
      changes = 0;
    }
  in
  row w ~id:(-1) ~log_lines:0 ~transient:true
    (Option.value w.summary ~default:"")

let scoped w =
  match w.scope.current () with
  | Some r when r.work == w && not (is_root r) -> Some r
  | _ -> None

let current w =
  match scoped w with Some r when is_running r -> Some r | _ -> None

let adopt p r =
  p.children <- r :: p.children;
  p.child_limit <- p.child_limit - 1;
  if p.child_limit <= 0 then begin
    (* Gone children are dropped from the list each time it doubles, so that
       adopting stays amortised constant time. *)
    p.children <- List.filter (fun c -> not c.gone) p.children;
    p.child_limit <- max 16 (List.length p.children)
  end

(* The label column of a depth is as wide as the labels it keeps, which
   only widen it, and the widest label each running task has drawn there,
   which leaves it once the task ends: a label that [set_label] replaced
   on a task that ended widens nothing. It keeps the label of each task
   that ended, its message if it has one, and the width it had when a row
   that cannot move was printed. *)
let grow_depths w d =
  let n = Array.length w.kept_cells in
  if d >= n then begin
    let kept = Array.make (d + 1) 0 in
    Array.blit w.kept_cells 0 kept 0 n;
    w.kept_cells <- kept;
    w.running_cells <-
      Array.init (d + 1) (fun i ->
          if i < n then w.running_cells.(i) else Hashtbl.create 4)
  end

let leave_column r =
  if r.label_cells >= 0 then begin
    let cells = r.work.running_cells.(depth r) in
    (match Hashtbl.find_opt cells r.label_cells with
    | Some n when n > 1 -> Hashtbl.replace cells r.label_cells (n - 1)
    | _ -> Hashtbl.remove cells r.label_cells);
    r.label_cells <- -1
  end

let widen_label r =
  let w = r.work and d = depth r in
  grow_depths w d;
  if is_running r then begin
    let c = max r.label_cells (Console_width.string_width r.label) in
    if c <> r.label_cells then begin
      leave_column r;
      let cells = w.running_cells.(d) in
      Hashtbl.replace cells c
        (1 + Option.value (Hashtbl.find_opt cells c) ~default:0);
      r.label_cells <- c
    end
  end
  else
    w.kept_cells.(d) <-
      max w.kept_cells.(d)
        (Console_width.string_width (Option.value r.message ~default:r.label))

let rec forget r =
  changed r;
  leave_column r;
  r.memo <- [];
  r.gone <- true;
  List.iter forget r.children;
  r.children <- [];
  Queue.clear r.output;
  r.output_bytes <- 0;
  r.held <- 0

let rec size r =
  List.fold_left (fun n c -> if c.gone then n else n + size c) 1 r.children

(* The ended tasks kept under a task weigh at most [held_cap] bytes, as
   [weight] estimates them. Past it, the oldest are forgotten until they weigh
   half of it; the task that just ended is kept. *)
let held_cap = 1_048_576

(* The bytes an ended task holds; a success drops its output. *)
let weight r =
  256 + String.length r.label
  + Option.fold ~none:0 ~some:String.length r.message
  + (if r.outcome = Some `Succeeded then 0 else r.output_bytes)
  + r.held

let shed p latest =
  let keep c =
    if
      p.held > held_cap / 2 && c != latest && (not (is_running c)) && not c.gone
    then begin
      p.held <- p.held - c.weight;
      p.shed <- p.shed + size c;
      changed p;
      forget c;
      false
    end
    else true
  in
  p.children <- List.rev (List.filter keep (List.rev p.children))

let hold r =
  match r.parent with
  | None -> ()
  | Some p ->
      r.weight <- weight r;
      p.held <- p.held + r.weight;
      if p.held > held_cap then shed p r

let child_outcome = function
  | `Succeeded -> `Succeeded
  | `Failed | `Cancelled -> `Cancelled

let ended outcome ?message r =
  let w = r.work in
  leave_column r;
  Queue.clear r.window;
  r.ended_at <- Some (w.now ());
  r.message <- Option.map Task_text.label message;
  r.outcome <- Some outcome;
  if not (is_root r) then begin
    w.finished <- w.finished + 1;
    match outcome with
    | `Failed -> w.failed <- w.failed + 1
    | `Cancelled -> w.cancelled <- w.cancelled + 1
    | `Succeeded -> ()
  end;
  Option.iter (fun k -> Hashtbl.remove w.by_key k) r.key;
  hold r;
  (* A success clears its output after the watchers saw its end. *)
  Fun.protect
    (fun () -> notify w (Ended r))
    ~finally:(fun () ->
      if outcome = `Succeeded then begin
        Queue.clear r.output;
        r.output_bytes <- 0
      end)

(* When a task ends, the [ending] function of its work runs first, then the
   running tasks under it end, in the order they started: they succeed if it
   succeeds, and are cancelled otherwise. A task ends once, and whole,
   whatever that function or a subtask raises. *)
let rec finish outcome ?message r =
  if is_running r && not r.finishing then begin
    r.finishing <- true;
    let children = List.rev (running_children r) in
    run_all
      ((fun () -> r.work.ending r)
       :: List.map (fun c () -> finish (child_outcome outcome) c) children
      @ [ (fun () -> ended outcome ?message r) ])
  end

let succeed ?message r = finish `Succeeded ?message r
let fail ?message r = finish `Failed ?message r
let cancel ?message r = finish `Cancelled ?message r

let start ?key ?(log_lines = 8) ?(transient = false) parent label =
  if log_lines < 0 then invalid_arg "Console.Task.start: negative log_lines";
  (match key with
  | Some "" -> invalid_arg "Console.Task.start: empty key"
  | _ -> ());
  let w = parent.work in
  match Option.bind key (Hashtbl.find_opt w.by_key) with
  | Some r -> (r, false)
  | None ->
      let id = w.next_id in
      w.next_id <- id + 1;
      let r = row w ~id ?key ~parent ~log_lines ~transient label in
      w.started <- w.started + 1;
      Option.iter (fun k -> Hashtbl.replace w.by_key k r) key;
      if not parent.gone then adopt parent r;
      notify w (Started r);
      (* A task started under an ended task ends at once: it succeeds if its
         parent succeeded, and is cancelled otherwise. *)
      (match parent.outcome with
      | None -> ()
      | Some `Succeeded -> succeed r
      | Some (`Failed | `Cancelled) -> cancel r);
      (r, true)

let within r f =
  let value = ref None in
  r.work.scope.within r (fun () -> value := Some (f ()));
  Option.get !value

(* A task found by its key is ended by the call that started it, not by this
   one. *)
let run ?key ?log_lines ?transient parent label f =
  match start ?key ?log_lines ?transient parent label with
  | r, false -> within r (fun () -> f r)
  | r, true -> (
      let value = ref None in
      match r.work.scope.within r (fun () -> value := Some (f r)) with
      | () ->
          succeed r;
          Option.get !value
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          if r.work.scope.cancelled e then cancel r
          else fail ~message:(r.label ^ ": " ^ Printexc.to_string e) r;
          Printexc.raise_with_backtrace e bt)

let set_label r label =
  if is_running r && not (is_root r) then begin
    r.label <- Task_text.label label;
    if r.label_cells >= 0 then widen_label r;
    notify r.work (Renamed r)
  end

let count_text = function
  | Spin -> ""
  | Count (c, tot) -> string_of_int c ^ "/" ^ string_of_int tot
  | Bytes (c, tot) -> Task_text.bytes c ^ " / " ^ Task_text.bytes tot

let rec digits n = if n < 10 then 1 else 1 + digits (n / 10)

(* The widest a count of items can get, given its total: [total/total], and at
   least [999/999]. *)
let items_width total = max (String.length "999/999") ((2 * digits total) + 1)

let rate_text metric rate =
  match metric with
  | Bytes _ when rate > 1. -> Task_text.bytes (Int64.of_float rate) ^ "/s"
  | Spin | Count _ | Bytes _ -> ""

let same_metric a b =
  match (a, b) with
  | Spin, Spin -> true
  | Count (c, n), Count (c', n') -> c = c' && n = n'
  | Bytes (c, n), Bytes (c', n') -> Int64.equal c c' && Int64.equal n n'
  | _ -> false

(* The rate is a moving average that weighs the past by the time since it was
   measured, with time constant [tau] seconds, not by the number of reports:
   reporting often or seldom gives the same rate for the same work. *)
let tau = 2.

let measure r cur =
  let now = r.work.now () in
  let dt = now -. r.last_t in
  if dt > 0. then begin
    let inst = Float.max 0. ((cur -. r.last_cur) /. dt) in
    r.rate <-
      (if r.rate <= 0. then inst
       else
         let keep = Float.exp (-.dt /. tau) in
         (keep *. r.rate) +. ((1. -. keep) *. inst));
    r.last_cur <- cur;
    r.last_t <- now
  end

(* The first task that counts bytes sets the count, short count and rate
   columns to the widest ones of bytes, which no later count or rate passes,
   as [Task_text.bytes] is never wider than "1023.99 KiB". A count of items
   widens the count column, short or not, to what its total needs, when the
   total changes. *)
let widest_bytes = String.length "1023.99 KiB / 1023.99 TiB"
let widest_short = String.length "999M/999M"
let widest_rate = String.length "1023.99 MiB/s"

let set_metric r metric =
  let moved = not (same_metric r.metric metric) in
  let w = r.work in
  (match (r.metric, metric) with
  | Count (_, n), Count (_, n') when n = n' -> ()
  | Bytes _, Bytes _ -> ()
  | _, Count (_, total) ->
      let cells = items_width total in
      w.count_cell <- max w.count_cell cells;
      w.short_cell <- max w.short_cell cells
  | _, Bytes _ ->
      w.count_cell <- max w.count_cell widest_bytes;
      w.short_cell <- max w.short_cell widest_short;
      w.rate_cell <- max w.rate_cell widest_rate
  | _, Spin -> ());
  changed r;
  r.metric <- metric;
  if moved then notify w (Progressed r)

let set_items r ~count ~total =
  if count < 0 || total < 0 then
    invalid_arg "Console.Task.set_items: negative progress";
  if is_running r && not (is_root r) then
    match r.metric with
    | Bytes _ -> invalid_arg "Console.Task.set_items: the task counts bytes"
    | Spin ->
        measure r (float_of_int count);
        set_metric r (Count (count, max count total))
    | Count (c, n) ->
        let count = max c count in
        measure r (float_of_int count);
        set_metric r (Count (count, max count (max n total)))

let set_bytes r ~count ~total =
  if count < 0L || total < 0L then
    invalid_arg "Console.Task.set_bytes: negative progress";
  if is_running r && not (is_root r) then begin
    let count, total =
      match r.metric with
      | Count _ -> invalid_arg "Console.Task.set_bytes: the task counts items"
      | Spin -> (count, Int64.max count total)
      | Bytes (c, n) ->
          let count = Int64.max c count in
          (count, Int64.max count (Int64.max n total))
    in
    measure r (Int64.to_float count);
    set_metric r (Bytes (count, total))
  end

(* The count saturates at the largest integer instead of overflowing. *)
let advance ?(by = 1) r =
  if by < 0 then invalid_arg "Console.Task.advance: negative progress";
  if is_running r && not (is_root r) then
    match r.metric with
    | Spin -> invalid_arg "Console.Task.advance: the task counts nothing"
    | Count (c, n) ->
        set_items r
          ~count:(if by > max_int - c then max_int else c + by)
          ~total:n
    | Bytes (c, n) ->
        let by = Int64.of_int by in
        let count =
          if Int64.compare by (Int64.sub Int64.max_int c) > 0 then Int64.max_int
          else Int64.add c by
        in
        set_bytes r ~count ~total:n

let fraction = function
  | Spin -> None
  | Count (cur, total) -> Some (float cur /. float (max 1 total))
  | Bytes (cur, total) ->
      Some (Int64.to_float cur /. Int64.to_float (Int64.max 1L total))

let eta r =
  let left =
    match r.metric with
    | Spin -> None
    | Count (cur, total) -> Some (float (total - cur))
    | Bytes (cur, total) -> Some (Int64.to_float (Int64.sub total cur))
  in
  match left with
  | Some left when r.rate > 0. && is_running r -> Some (left /. r.rate)
  | _ -> None

(* A task keeps at most [output_cap] bytes of output to show if it fails,
   dropping the oldest lines first. *)
let output_cap = 1_048_576

let keep_output r line =
  Queue.add line r.output;
  r.output_bytes <- r.output_bytes + String.length line.text + 1;
  while r.output_bytes > output_cap do
    let old = Queue.take r.output in
    r.output_bytes <- r.output_bytes - String.length old.text - 1;
    r.dropped <- r.dropped + 1
  done

let push r line =
  if r.log_lines > 0 then begin
    Queue.add line r.window;
    if Queue.length r.window > r.log_lines then ignore (Queue.take r.window)
  end

(* A running task keeps each line, capped, in its window and its output. The
   watchers see every line, uncapped, also from an ended task. *)
let add r kind text =
  let w = r.work in
  List.iter
    (fun text ->
      let now = w.now () in
      let line = { gap = now -. r.log_prev; kind; text; seq = w.next_line } in
      w.next_line <- w.next_line + 1;
      r.log_prev <- now;
      if is_running r then begin
        let kept =
          let text = Task_text.capped line.text in
          if text == line.text then line else { line with text }
        in
        push r kept;
        keep_output r kept
      end;
      notify w (Line (r, line)))
    (Task_text.lines text)

(* A line of the root belongs to no task: it is not kept, and its [gap] and
   [seq] are [0]. *)
let above root kind text =
  List.iter
    (fun text ->
      notify root.work (Line (root, { gap = 0.; kind; text; seq = 0 })))
    (Task_text.lines text)

let kind_of = function None -> Output | Some l -> Logs l

let log ?level r text =
  if is_root r then above r (kind_of level) text
  else if is_running r then add r (kind_of level) text

let report root level text =
  match scoped root.work with
  | Some r -> add r (Logs level) text
  | None -> above root (Logs level) text

let event (ch : change) =
  let task r = if is_root r then None else Some (r.id, path r) in
  let parent r =
    match r.parent with Some p when not (is_root p) -> Some p.id | _ -> None
  in
  fun ~time ->
    match ch with
    | Started r -> Some (Event.start ?parent:(parent r) ~time (r.id, path r))
    | Progressed r -> (
        let progress count total unit =
          Some (Event.progress ~time ~count ~total ~unit (r.id, path r))
        in
        match r.metric with
        | Spin -> None
        | Count (cur, total) ->
            progress (Int64.of_int cur) (Int64.of_int total) `Items
        | Bytes (cur, total) -> progress cur total `Bytes)
    | Renamed r -> Some (Event.label ~time (r.id, path r) r.label)
    | Line (r, line) ->
        let level =
          match line.kind with
          | Output -> None
          | Logs l -> Some (l :> Event.level)
        in
        Some (Event.output ?task:(task r) ?level ~time line.text)
    | Ended r ->
        let e =
          match Option.value r.outcome ~default:`Cancelled with
          | `Succeeded -> Event.succeeded
          | `Failed -> Event.failed
          | `Cancelled -> Event.cancelled
        in
        Some (e ?task:(task r) ?message:r.message ~time ~elapsed:(elapsed r) ())

let on_event r f =
  let w = r.work in
  watch w (fun ch ->
      match event ch ~time:(since_start w) with Some e -> f e | None -> ())

let state r : [ `Running | outcome ] =
  match r.outcome with
  | None -> `Running
  | Some o -> (o :> [ `Running | outcome ])

let pp ppf r =
  Fmt.pf ppf "<task %d %S%s>" r.id r.label
    (match r.outcome with
    | None -> ""
    | Some `Succeeded -> " succeeded"
    | Some `Failed -> " failed"
    | Some `Cancelled -> " cancelled")

let id r = r.id
let work r = r.work
let parent r = r.parent
let label r = r.label
let metric r = r.metric
let transient r = r.transient
let message r = r.message
let outcome r = r.outcome
let window r = r.window
let output r = r.output
let dropped r = r.dropped
let gone r = r.gone
let shed r = r.shed
let rate r = r.rate
let version r = r.version
let memo r = r.memo

module Work = struct
  let now (w : work) = w.now
  let summary (w : work) = w.summary
  let started (w : work) = w.started
  let finished (w : work) = w.finished
  let failed (w : work) = w.failed
  let cancelled (w : work) = w.cancelled
  let count_cell (w : work) = w.count_cell
  let short_cell (w : work) = w.short_cell
  let rate_cell (w : work) = w.rate_cell

  let label_cell (w : work) depth =
    if depth < Array.length w.kept_cells then
      Hashtbl.fold
        (fun c _ n -> max c n)
        w.running_cells.(depth) w.kept_cells.(depth)
    else 0

  let depths (w : work) = Array.length w.kept_cells

  let hold_labels (w : work) =
    Array.iteri (fun d _ -> w.kept_cells.(d) <- label_cell w d) w.kept_cells
end
