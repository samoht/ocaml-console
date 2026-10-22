(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state
module Layout = Display_layout

type nonrec t = t
type task = row
type nonrec scope = scope
type nonrec level = level

let scope ~current ~within ~cancelled ~write_error =
  { current; within; cancelled; write_error }

(* Without a driver, the current task is that of the innermost [with_task]
   that runs. *)
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
    write_error = (fun _ -> false);
  }

let pp = Display_state.pp

(* [tapped out note] is [out] calling [note] with the last byte of each
   write, a space for blanks. *)
let tapped (out : Format.formatter_out_functions) note =
  let out_string s o l =
    if l > 0 then note s.[o + l - 1];
    out.out_string s o l
  in
  let out_newline () =
    note '\n';
    out.out_newline ()
  in
  let blank write n =
    if n > 0 then note ' ';
    write n
  in
  {
    out with
    out_string;
    out_newline;
    out_spaces = blank out.out_spaces;
    out_indent = blank out.out_indent;
  }

(* [tap ppf note] makes [ppf] call [note] as {!tapped} does, and is the out
   functions [ppf] had and those it has. *)
let tap ppf note =
  let out = Format.pp_get_formatter_out_functions ppf () in
  let ours = tapped out note in
  Format.pp_set_formatter_out_functions ppf ours;
  (out, ours)

let note_line t c = t.partial <- c <> '\n'

(* Without a terminal, what the program and the display write on [ppf] is
   watched for its last byte, so that a line the display prints starts at
   the first column. *)
let watch_lines t =
  Format.pp_print_flush t.ppf ();
  let out, ours = tap t.ppf (note_line t) in
  t.release <-
    (fun () ->
      let now = Format.pp_get_formatter_out_functions t.ppf () in
      if now.out_string == ours.out_string then
        Format.pp_set_formatter_out_functions t.ppf out)

(* The size in which a display redraws its rows, if it does, and the lines
   it prints otherwise. *)
let shape ?size = function
  | `Auto -> (size, Plain)
  | `Tty -> (Some (Option.value size ~default:(fun () -> (80, 24))), Plain)
  | `Plain -> (None, Plain)
  | `Quiet -> (None, Quiet)
  | `Json -> (None, Json)

let row t ~id ?key ?parent ~log_lines ~transient label =
  let now = t.now () in
  {
    id;
    key;
    disp = t;
    parent;
    log_lines;
    window = Queue.create ();
    output = Queue.create ();
    output_bytes = 0;
    dropped = 0;
    log_prev = now;
    label = Display_text.label label;
    metric = Spin;
    transient;
    started_at = now;
    ended_at = None;
    message = None;
    outcome = None;
    children = [];
    child_limit = 16;
    gone = false;
    held = 0;
    weight = 0;
    shed = 0;
    last_cur = 0L;
    last_t = now;
    rate = 0.;
  }

let v ?(layout = Layout.default) ?summary ?view ?(keep_ended = false) ?size
    ?(progress = `Auto) ?scope ~now ppf =
  let size, form = shape ?size progress in
  let depth =
    match Fmt.style_renderer ppf with
    | `Ansi_tty -> Some (Color.depth ppf)
    | `None -> None
  in
  let t =
    {
      ppf;
      size;
      form;
      depth;
      now;
      summary = Option.map Display_text.clean summary;
      layout;
      keep_ended;
      start = now ();
      width = 80;
      height = 24;
      running = Queue.create ();
      stale = 0;
      count_cell = 0;
      by_key = Hashtbl.create 16;
      next_id = 0;
      next_line = 0;
      started = 0;
      finished = 0;
      failed = 0;
      cancelled = 0;
      result = None;
      view;
      frame = [||];
      frame_widths = [||];
      painted_at = neg_infinity;
      changes = 0;
      painted = (-1, -1, -1, -1);
      closed = false;
      closing = false;
      suspended = 0;
      partial = false;
      writes = Queue.create ();
      writing = false;
      release = ignore;
      ending = ignore;
      root = None;
      scope = (match scope with Some s -> s | None -> local ());
    }
  in
  t.root <- Some (row t ~id:(-1) ~log_lines:0 ~transient:true "");
  ignore (Display_region.refresh_size t);
  if is_live t then begin
    Display_region.write t
      (Display_region.hide_cursor ^ Display_region.fresh_row t);
    Display_region.paint t
  end
  else watch_lines t;
  t

(* What a driver sends on to [t]'s output past [t.ppf] is watched too. *)
let watch t out = tapped out (note_line t)
let redraw t = Display_region.paint t

let touch t =
  t.changes <- t.changes + 1;
  if is_live t then Display_region.paint_if_stale t

let end_line t (out : Format.formatter_out_functions) last =
  try
    Format.pp_print_flush t.ppf ();
    if !last <> '\n' then begin
      let eol = Display_region.(synchronized t (eol t)) in
      out.out_string eol 0 (String.length eol);
      out.out_flush ()
    end
  with e when write_error t e -> ()

(* What [f] writes is the output of no task, as events. *)
let events t f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_utf_8 ppf (Fmt.utf_8 t.ppf);
  Format.pp_set_margin ppf t.width;
  Fun.protect
    (fun () -> f ppf)
    ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      List.iter
        (fun text ->
          Display_event.output t None { gap = 0.; kind = Output; text; seq = 0 })
        (Display_text.lines (Console_text.strip (Buffer.contents buf))))

(* What [f] writes is watched for its last byte: output that does not end
   its line is ended, so the region starts on a line of its own. *)
let with_output t f =
  let call () =
    Format.pp_print_flush t.ppf ();
    let margin = Format.pp_get_margin t.ppf () in
    let last = ref '\n' in
    let out, _ = tap t.ppf (( := ) last) in
    Format.pp_set_margin t.ppf t.width;
    Fun.protect
      (fun () -> f t.ppf)
      ~finally:(fun () ->
        end_line t out last;
        Format.pp_set_formatter_out_functions t.ppf out;
        Format.pp_set_margin t.ppf margin)
  in
  if t.closed then f t.ppf
  else if Display_event.is_json t then events t f
  else if (not (is_live t)) || t.suspended > 0 then call ()
  else begin
    ignore (Display_region.refresh_size t);
    Display_region.erase t;
    t.suspended <- t.suspended + 1;
    Fun.protect call ~finally:(fun () ->
        t.suspended <- t.suspended - 1;
        Display_region.paint t)
  end

let set_view t view =
  if not t.closed then begin
    t.view <- Some view;
    t.changes <- t.changes + 1;
    Display_region.paint t
  end

let children = children
let tree = Display_tree.tree
let summary = Display_tree.summary
let default_view = Display_tree.default_view

(* Lines printed once: above the region, or in turn without one. *)
let print t = function [] -> touch t | lines -> Display_region.commit t lines

(* {1 Tasks} *)

(* The root is current where no other task is. *)
let scoped t =
  match t.scope.current () with
  | Some r when r.disp == t && not (is_root r) -> Some r
  | _ -> None

let current t =
  match scoped t with Some r when is_running r -> Some r | _ -> None

(* The lines a task keeps for its failure, the oldest dropped first. *)
let output_cap = 1_048_576

let keep_output r line =
  Queue.add line r.output;
  r.output_bytes <- r.output_bytes + String.length line.text + 1;
  while r.output_bytes > output_cap do
    let old = Queue.take r.output in
    r.output_bytes <- r.output_bytes - String.length old.text - 1;
    r.dropped <- r.dropped + 1
  done

(* A window holds the latest lines its task shows. *)
let push r line =
  if r.log_lines > 0 then begin
    Queue.add line r.window;
    if Queue.length r.window > r.log_lines then ignore (Queue.take r.window)
  end

(* A running task takes each line in its window and, on a terminal or
   quiet, keeps it for its failure; otherwise, and for a task that ended, a
   line is printed at once after its label, or is an event. *)
let add r kind text =
  let t = r.disp in
  let line text =
    let now = t.now () in
    let line = { gap = now -. r.log_prev; kind; text; seq = t.next_line } in
    t.next_line <- t.next_line + 1;
    r.log_prev <- now;
    if Display_event.is_json t then begin
      Display_event.output t (Some r) line;
      []
    end
    else if not (is_running r) then Display_row.labelled r line
    else begin
      let kept =
        let text = Display_text.capped line.text in
        if text == line.text then line else { line with text }
      in
      push r kept;
      if keeps_output t then begin
        keep_output r kept;
        []
      end
      else Display_row.labelled r line
    end
  in
  print t (List.concat_map line (Display_text.lines text))

(* A line of the root goes above the region at once, as a message of no
   task does. *)
(* Lines of no task: above the region, in turn, or events. *)
let above t kind text =
  let lines = Display_text.lines text in
  if Display_event.is_json t then
    List.iter
      (fun text ->
        Display_event.output t None { gap = 0.; kind; text; seq = 0 })
      lines
  else
    let level = match kind with Logs l -> l | Output -> `App in
    print t (List.concat_map (Display_row.message t level) lines)

let log r text =
  let t = r.disp in
  if t.closed then ()
  else if is_root r then above t Output text
  else if is_running r then add r Output text

let report t level text =
  if not t.closed then
    match scoped t with
    | Some r -> add r (Logs level) text
    | None -> above t (Logs level) text

(* The output a task kept, under its line. *)
let kept r =
  let all = Queue.fold (fun acc l -> l :: acc) [] r.output in
  (if r.dropped > 0 then [ Display_row.dropped r r.dropped ] else [])
  @ List.concat_map (Display_row.under r) (List.rev all)

(* What a task prints when it ends without a terminal: quiet, its line and
   all its lines if it failed or was cancelled, a transient task too, since
   nothing else shows the failure; in turn, its line alone, its lines having
   been printed as they came. Its event says the rest. *)
let ending r outcome =
  let t = r.disp in
  let failed =
    match outcome with `Succeeded -> false | `Failed | `Cancelled -> true
  in
  match t.form with
  | Json -> []
  | Quiet -> if failed then Display_row.appended r :: kept r else []
  | Plain -> if r.transient then [] else [ Display_row.appended r ]

(* {2 The scrollback}

   On a terminal, a task that ended waits in the region while a task above
   it runs, or with [keep_ended] until the display closes, and goes to the
   scrollback with its top task, as its tree. *)

let rec running_above r =
  match r.parent with
  | None -> false
  | Some p -> is_running p || running_above p

let parent_of r = match r.parent with Some p -> p | None -> root r.disp

let adopt p r =
  p.children <- r :: p.children;
  p.child_limit <- p.child_limit - 1;
  if p.child_limit <= 0 then begin
    (* The children that are gone are forgotten each time the list doubles. *)
    p.children <- List.filter (fun c -> not c.gone) p.children;
    p.child_limit <- max 16 (List.length p.children)
  end

let rec release r =
  r.gone <- true;
  List.iter release r.children;
  r.children <- [];
  Queue.clear r.output;
  r.output_bytes <- 0;
  r.held <- 0

let rec size r =
  List.fold_left (fun n c -> if c.gone then n else n + size c) 1 r.children

(* The tasks that wait under a task are bounded: past 1 MiB, the oldest
   are dropped, down to half of it, never the latest. *)
let held_cap = 1_048_576

let weight r =
  256 + String.length r.label
  + Option.fold ~none:0 ~some:String.length r.message
  + r.output_bytes + r.held

let shed p latest =
  let keep c =
    if
      p.held > held_cap / 2 && c != latest && (not (is_running c)) && not c.gone
    then begin
      p.held <- p.held - c.weight;
      p.shed <- p.shed + size c;
      release c;
      false
    end
    else true
  in
  p.children <- List.rev (List.filter keep (List.rev p.children))

let hold r =
  let p = parent_of r in
  r.weight <- weight r;
  p.held <- p.held + r.weight;
  if p.held > held_cap then shed p r

(* A top task that ends, unless [keep_ended], goes to the scrollback as its
   tree. *)
let moved r =
  let t = r.disp in
  if t.keep_ended || running_above r then begin
    hold r;
    []
  end
  else
    let lines = Display_tree.scrollback t ~width:(room t) r in
    release r;
    lines

(* The tasks under one that ends end first: they succeed with it, and are
   cancelled when it fails or is cancelled. *)
let rec finish outcome ?message r =
  if is_root r || not (is_running r) then []
  else begin
    let t = r.disp in
    t.ending r;
    let child_outcome =
      match outcome with
      | `Succeeded -> `Succeeded
      | `Failed | `Cancelled -> `Cancelled
    in
    let children =
      List.concat_map (finish child_outcome) (List.rev (running_children r))
    in
    Queue.clear r.window;
    r.ended_at <- Some (t.now ());
    r.message <- Option.map Display_text.label message;
    r.outcome <- Some outcome;
    t.finished <- t.finished + 1;
    (match outcome with
    | `Failed -> t.failed <- t.failed + 1
    | `Cancelled -> t.cancelled <- t.cancelled + 1
    | `Succeeded -> ());
    Option.iter (fun k -> Hashtbl.remove t.by_key k) r.key;
    if Display_event.is_json t then Display_event.ended r;
    t.stale <- t.stale + 1;
    compact t;
    if is_live t then begin
      (* A success folds its output away. *)
      if outcome = `Succeeded then begin
        Queue.clear r.output;
        r.output_bytes <- 0
      end;
      children @ moved r
    end
    else begin
      let lines = ending r outcome in
      release r;
      children @ lines
    end
  end

let start t r =
  t.started <- t.started + 1;
  Queue.add r t.running;
  Option.iter (fun k -> Hashtbl.replace t.by_key k r) r.key;
  if is_plain t && not r.transient then print t [ Display_row.started r ];
  if Display_event.is_json t then Display_event.started r;
  let p = parent_of r in
  if not p.gone then adopt p r;
  if is_running p then touch t
  else
    (* A task started under one that ended takes its ending. *)
    match p.outcome with
    | Some `Succeeded -> print t (finish `Succeeded r)
    | _ -> print t (finish `Cancelled r)

(* The task of [key] that runs, or a new task, and whether it is new. *)
let keyed ?key ?parent ?(log_lines = 8) ?(transient = false) t label =
  if log_lines < 0 then invalid_arg "Console.Display.task: negative log_lines";
  (match key with
  | Some "" -> invalid_arg "Console.Display.task: empty key"
  | _ -> ());
  (match parent with
  | Some p when p.disp != t ->
      invalid_arg "Console.Display.task: parent belongs to another display"
  | _ -> ());
  match Option.bind key (Hashtbl.find_opt t.by_key) with
  | Some r -> (r, false)
  | None ->
      let parent =
        match parent with
        | Some p when is_root p -> None
        | Some _ -> parent
        | None -> scoped t
      in
      let id = t.next_id in
      t.next_id <- id + 1;
      let r = row t ~id ?key ?parent ~log_lines ~transient label in
      if t.closed then begin
        (* A task of a closed display has already ended, so it ignores all. *)
        r.ended_at <- Some r.started_at;
        r.outcome <- Some `Cancelled
      end
      else start t r;
      (r, true)

let task ?key ?parent ?log_lines ?transient t label =
  fst (keyed ?key ?parent ?log_lines ?transient t label)

let root = root

let within r f =
  let value = ref None in
  r.disp.scope.within r (fun () -> value := Some (f ()));
  Option.get !value

let succeed ?message r = print r.disp (finish `Succeeded ?message r)
let fail ?message r = print r.disp (finish `Failed ?message r)
let cancel ?message r = print r.disp (finish `Cancelled ?message r)

(* A task found by its key ends as the call that started it says. *)
let with_task ?key ?parent ?log_lines ?transient t label f =
  match keyed ?key ?parent ?log_lines ?transient t label with
  | r, false -> within r (fun () -> f r)
  | r, true -> (
      let value = ref None in
      match t.scope.within r (fun () -> value := Some (f r)) with
      | () ->
          succeed r;
          Option.get !value
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          if t.scope.cancelled e then cancel r
          else fail ~message:(r.label ^ ": " ^ Printexc.to_string e) r;
          Printexc.raise_with_backtrace e bt)

let set_label r label =
  if is_running r && not (is_root r) then begin
    r.label <- Display_text.label label;
    if Display_event.is_json r.disp then Display_event.renamed r;
    touch r.disp
  end

let same_metric a b =
  match (a, b) with
  | Spin, Spin -> true
  | Count (c, n), Count (c', n') -> c = c' && n = n'
  | Bytes (c, n), Bytes (c', n') -> Int64.equal c c' && Int64.equal n n'
  | _ -> false

let set_metric r metric =
  let changed = not (same_metric r.metric metric) in
  r.metric <- metric;
  if changed && Display_event.is_json r.disp then Display_event.progress r;
  r.disp.count_cell <- max r.disp.count_cell (count_width metric);
  touch r.disp

let set_count r ~cur ~total =
  if cur < 0 || total < 0 then
    invalid_arg "Console.Display.set_count: negative progress";
  if is_running r && not (is_root r) then
    match r.metric with
    | Bytes _ -> invalid_arg "Console.Display.set_count: the task counts bytes"
    | Spin -> set_metric r (Count (cur, max cur total))
    | Count (c, n) ->
        let cur = max c cur in
        set_metric r (Count (cur, max cur (max n total)))

let iter t label f items =
  let total = List.length items in
  with_task t label (fun r ->
      set_count r ~cur:0 ~total;
      List.iteri
        (fun i x ->
          f x;
          set_count r ~cur:(i + 1) ~total)
        items)

let set_bytes r ~cur ~total =
  if cur < 0L || total < 0L then
    invalid_arg "Console.Display.set_bytes: negative progress";
  if is_running r && not (is_root r) then begin
    let cur, total =
      match r.metric with
      | Count _ ->
          invalid_arg "Console.Display.set_bytes: the task counts items"
      | Spin -> (cur, Int64.max cur total)
      | Bytes (c, n) ->
          let cur = Int64.max c cur in
          (cur, Int64.max cur (Int64.max n total))
    in
    let now = r.disp.now () in
    let dt = now -. r.last_t in
    if dt > 0. then begin
      let inst =
        Float.max 0. (Int64.to_float (Int64.sub cur r.last_cur) /. dt)
      in
      r.rate <-
        (if r.rate <= 0. then inst else (0.3 *. inst) +. (0.7 *. r.rate));
      r.last_cur <- cur;
      r.last_t <- now
    end;
    set_metric r (Bytes (cur, total))
  end

(* On a terminal, each top task ends with the tasks under it, in the order
   they started, so that the scrollback reads in that order. *)
let finish_running outcome ?message t =
  let rows =
    if is_live t then List.filter is_running (children (root t))
    else List.rev (running_rows t)
  in
  List.iter (fun r -> print t (finish outcome ?message r)) rows

let set_result ?detail t result =
  if (not t.closed) && Option.is_none t.result then begin
    (match result with
    | Ok _ -> finish_running `Succeeded t
    | Error _ -> finish_running `Cancelled t);
    let result =
      Result.map Display_text.clean result
      |> Result.map_error Display_text.clean
    in
    let detail =
      match detail with None -> [] | Some d -> Display_text.lines d
    in
    t.result <- Some (result, detail);
    touch t
  end

(* {1 Closing} *)

let close t =
  if not t.closed then begin
    (* Closed first, so that the tasks it ends draw no region. *)
    t.closed <- true;
    finish_running `Cancelled t;
    let final =
      match t.form with
      | Plain ->
          ignore (Display_region.refresh_size t);
          Display_region.final t
      | Quiet | Json -> []
    in
    let lines =
      match t.result with
      | None -> final
      | Some (r, d) -> final @ Display_row.detail t d @ Display_row.verdict t r
    in
    t.view <- None;
    List.iter release (root t).children;
    (root t).children <- [];
    let render l = Display_region.render t l ^ Display_region.eol t in
    let out = String.concat "" (List.map render lines) in
    if is_live t then begin
      let buf = Buffer.create 256 in
      Display_region.clear buf t;
      Buffer.add_string buf Display_region.show_cursor;
      Buffer.add_string buf out;
      Display_region.write t (Buffer.contents buf)
    end
    else begin
      if Display_event.is_json t then Display_event.result t
      else Display_region.write t out;
      t.release ()
    end
  end
