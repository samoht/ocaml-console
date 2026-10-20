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

let scope ~current ~within ~cancelled = { current; within; cancelled }

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
  }

let pp = Display_state.pp

(* [tap ppf note] makes [ppf] call [note] with the last byte of each write, a
   space for blanks, and is the out functions [ppf] had and those it has. *)
let tap ppf note =
  let out = Format.pp_get_formatter_out_functions ppf () in
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
  let ours =
    {
      out with
      out_string;
      out_newline;
      out_spaces = blank out.out_spaces;
      out_indent = blank out.out_indent;
    }
  in
  Format.pp_set_formatter_out_functions ppf ours;
  (out, ours)

(* Without a terminal, what the program and the display write on [ppf] is
   watched for its last byte, so that a line the display prints starts at
   the first column. *)
let watch_lines t =
  Format.pp_print_flush t.ppf ();
  let out, ours = tap t.ppf (fun c -> t.partial <- c <> '\n') in
  t.release <-
    (fun () ->
      let now = Format.pp_get_formatter_out_functions t.ppf () in
      if now.out_string == ours.out_string then
        Format.pp_set_formatter_out_functions t.ppf out)

let v ?(layout = Layout.default) ?header ?size ?scope ~now ppf =
  let depth =
    match Fmt.style_renderer ppf with
    | `Ansi_tty -> Some (Color.depth ppf)
    | `None -> None
  in
  let t =
    {
      ppf;
      size;
      depth;
      now;
      header = Option.map Display_text.clean header;
      layout;
      start = now ();
      width = 80;
      height = 24;
      running = Queue.create ();
      stale = 0;
      count_cell = 0;
      by_key = Hashtbl.create 16;
      next_id = 0;
      started = 0;
      finished = 0;
      failed = 0;
      cancelled = 0;
      result = None;
      scene = None;
      frame = [||];
      frame_widths = [||];
      painted_at = neg_infinity;
      changes = 0;
      painted = (-1, -1, -1, -1);
      closed = false;
      suspended = 0;
      partial = false;
      release = ignore;
      root = None;
      scope = (match scope with Some s -> s | None -> local ());
    }
  in
  ignore (Display_region.refresh_size t);
  if is_live t then begin
    Display_region.write t
      (Display_region.hide_cursor ^ Display_region.fresh_row t);
    Display_region.paint t
  end
  else watch_lines t;
  t

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
  with Sys_error _ -> ()

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
  else if (not (is_live t)) || t.suspended > 0 then call ()
  else begin
    ignore (Display_region.refresh_size t);
    Display_region.erase t;
    t.suspended <- t.suspended + 1;
    Fun.protect call ~finally:(fun () ->
        t.suspended <- t.suspended - 1;
        Display_region.paint t)
  end

let set_scene t scene =
  if not t.closed then begin
    t.scene <- Some scene;
    touch t
  end

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

(* A running task takes each line in its window and, on a terminal, keeps
   it for its failure; without a terminal, and for a task that ended, a
   line is printed at once after its label. *)
let add r kind text =
  let t = r.disp in
  let line text =
    let now = t.now () in
    let line = { gap = now -. r.log_prev; kind; text } in
    r.log_prev <- now;
    if not (is_running r) then Display_row.labelled r line
    else begin
      push r line;
      if is_live t then begin
        keep_output r line;
        []
      end
      else Display_row.labelled r line
    end
  in
  print t (List.concat_map line (Display_text.lines text))

(* A line of the root goes above the region at once, as a message of no
   task does. *)
let log r text =
  let t = r.disp in
  if t.closed then ()
  else if is_root r then
    print t
      (List.concat_map (Display_row.message t `App) (Display_text.lines text))
  else if is_running r then add r Output text

let report t level text =
  if not t.closed then
    match scoped t with
    | Some r -> add r (Logs level) text
    | None ->
        print t
          (List.concat_map
             (Display_row.message t level)
             (Display_text.lines text))

(* What a task prints when it ends: on a terminal, its line, then all its
   lines if it failed or was cancelled; without one, its line, then its
   window again if it failed. *)
let ending r outcome window =
  let t = r.disp in
  if r.transient then []
  else if is_live t then
    let block =
      match outcome with
      | `Succeeded -> []
      | `Failed | `Cancelled ->
          let all = Queue.fold (fun acc l -> l :: acc) [] r.output in
          (if r.dropped > 0 then [ Display_row.dropped r r.dropped ] else [])
          @ List.concat_map (Display_row.under r) (List.rev all)
    in
    Display_row.committed t r @ block
  else
    Display_row.appended r
    ::
    (match outcome with
    | `Failed -> List.concat_map (Display_row.under r) window
    | `Succeeded | `Cancelled -> [])

(* The tasks under one that ends end first: they succeed with it, and are
   cancelled when it fails or is cancelled. *)
let rec finish outcome ?message r =
  if is_root r || not (is_running r) then []
  else begin
    let t = r.disp in
    let child_outcome =
      match outcome with
      | `Succeeded -> `Succeeded
      | `Failed | `Cancelled -> `Cancelled
    in
    let children =
      List.concat_map (finish child_outcome) (List.rev (running_children r))
    in
    r.children <- [];
    let window = List.rev (Queue.fold (fun acc l -> l :: acc) [] r.window) in
    Queue.clear r.window;
    r.ended_at <- Some (t.now ());
    r.message <- Option.map Display_text.clean message;
    r.outcome <- Some outcome;
    t.finished <- t.finished + 1;
    (match outcome with
    | `Failed -> t.failed <- t.failed + 1
    | `Cancelled -> t.cancelled <- t.cancelled + 1
    | `Succeeded -> ());
    Option.iter (fun k -> Hashtbl.remove t.by_key k) r.key;
    t.stale <- t.stale + 1;
    compact t;
    let lines = ending r outcome window in
    Queue.clear r.output;
    r.output_bytes <- 0;
    children @ lines
  end

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
    label = Display_text.clean label;
    metric = Spin;
    transient;
    started_at = now;
    ended_at = None;
    message = None;
    outcome = None;
    children = [];
    child_limit = 16;
    last_cur = 0L;
    last_t = now;
    rate = 0.;
  }

let start t r =
  t.started <- t.started + 1;
  Queue.add r t.running;
  Option.iter (fun k -> Hashtbl.replace t.by_key k r) r.key;
  if not (is_live t || r.transient) then print t [ Display_row.started r ];
  match r.parent with
  | Some p when is_running p ->
      (* The children that ended are pruned each time the list doubles. *)
      p.children <- r :: p.children;
      p.child_limit <- p.child_limit - 1;
      if p.child_limit <= 0 then begin
        p.children <- List.filter is_running p.children;
        p.child_limit <- max 16 (List.length p.children)
      end;
      touch t
  | Some p -> (
      (* A task started under one that ended takes its ending. *)
      match p.outcome with
      | Some `Succeeded -> print t (finish `Succeeded r)
      | _ -> print t (finish `Cancelled r))
  | None -> touch t

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

let root t =
  match t.root with
  | Some r -> r
  | None ->
      let r = row t ~id:(-1) ~log_lines:0 ~transient:true "" in
      t.root <- Some r;
      r

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
          if t.scope.cancelled e then cancel r else fail r;
          Printexc.raise_with_backtrace e bt)

let set_label r label =
  if is_running r && not (is_root r) then begin
    r.label <- Display_text.clean label;
    touch r.disp
  end

let set_metric r metric =
  r.metric <- metric;
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

let finish_running outcome ?message t =
  List.iter
    (fun r -> print t (finish outcome ?message r))
    (List.rev (running_rows t))

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
    (* The scene goes with the display, and cannot keep it from closing. *)
    t.scene <- None;
    finish_running `Cancelled t;
    t.closed <- true;
    let header = Option.to_list (Display_row.header t) in
    let verdict, detail =
      match t.result with
      | None -> ([], [])
      | Some (r, d) -> (Display_row.verdict t r, Display_row.detail t d)
    in
    let lines = header @ detail @ verdict in
    let render l = Display_region.render t l ^ Display_region.eol t in
    if is_live t then begin
      ignore (Display_region.refresh_size t);
      let buf = Buffer.create 256 in
      Display_region.clear buf t;
      Buffer.add_string buf Display_region.show_cursor;
      List.iter (fun l -> Buffer.add_string buf (render l)) lines;
      Display_region.write t (Buffer.contents buf)
    end
    else begin
      Display_region.write t (String.concat "" (List.map render lines));
      t.release ()
    end
  end
