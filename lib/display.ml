(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state
module Line = Display_line

type nonrec t = t
type task = row

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

let v ?(layout = Line.default) ?header ?size ~now ppf =
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
      frame = [||];
      frame_widths = [||];
      painted_at = neg_infinity;
      changes = 0;
      painted = (-1, -1, -1, -1);
      closed = false;
      suspended = 0;
      partial = false;
      release = ignore;
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

let check_open fn t =
  if t.closed then invalid_arg ("Console.Display." ^ fn ^ ": closed display")

let redraw t =
  check_open "redraw" t;
  Display_region.paint t

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
  check_open "with_output" t;
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
  if (not (is_live t)) || t.suspended > 0 then call ()
  else begin
    ignore (Display_region.refresh_size t);
    Display_region.erase t;
    t.suspended <- t.suspended + 1;
    Fun.protect call ~finally:(fun () ->
        t.suspended <- t.suspended - 1;
        Display_region.paint t)
  end

(* Lines printed once: above the region, or in turn without one. *)
let print t lines = Display_region.commit t lines

let message ?(level = `Info) t text =
  check_open "message" t;
  let lines = match Display_text.lines text with [] -> [ "" ] | l -> l in
  print t
    (List.concat_map
       (fun l ->
         let line = Display_row.message t level l in
         if is_live t then Span.wrap (max 1 t.width) line else [ line ])
       lines)

(* {1 Tasks} *)

let rec finish ?(swept = false) outcome ?message r =
  let t = r.disp in
  if is_running r then begin
    let child_outcome =
      match outcome with
      | `Succeeded -> `Succeeded
      | `Failed | `Cancelled -> `Cancelled
    in
    List.iter
      (fun child -> finish ~swept:true child_outcome child)
      (running_children r);
    r.children <- [];
    (* The row leaves the region: its tail is printed before its line. *)
    let tail =
      Queue.fold
        (fun acc log ->
          if log.kept then Display_row.committed_log r log :: acc else acc)
        [] r.logs
      |> List.rev
    in
    Queue.clear r.logs;
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
    let line =
      if swept || r.transient then []
      else if is_live t then Display_row.committed t r
      else [ Display_row.appended r ]
    in
    match tail @ line with [] -> touch t | lines -> print t lines
  end

let task ?key ?parent ?(log_lines = 8) ?(transient = false) t label =
  check_open "task" t;
  if log_lines < 0 then invalid_arg "Console.Display.task: negative log_lines";
  (match key with
  | Some "" -> invalid_arg "Console.Display.task: empty key"
  | _ -> ());
  (match parent with
  | Some p when p.disp != t ->
      invalid_arg "Console.Display.task: parent belongs to another display"
  | _ -> ());
  match Option.bind key (Hashtbl.find_opt t.by_key) with
  | Some r -> r
  | None ->
      let now = t.now () in
      let id = t.next_id in
      t.next_id <- id + 1;
      let r =
        {
          id;
          key;
          disp = t;
          parent;
          log_lines;
          logs = Queue.create ();
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
      in
      t.started <- t.started + 1;
      Queue.add r t.running;
      Option.iter (fun k -> Hashtbl.replace t.by_key k r) key;
      (match parent with
      | Some p when is_running p ->
          (* The children that ended are pruned each time the list doubles. *)
          p.children <- r :: p.children;
          p.child_limit <- p.child_limit - 1;
          if p.child_limit <= 0 then begin
            p.children <- List.filter is_running p.children;
            p.child_limit <- max 16 (List.length p.children)
          end
      | Some p -> (
          (* A task started under one that ended takes its ending. *)
          match p.outcome with
          | Some `Succeeded -> finish ~swept:true `Succeeded r
          | _ -> finish ~swept:true `Cancelled r)
      | None -> ());
      touch t;
      r

let set_label r label =
  check_open "set_label" r.disp;
  if is_running r then begin
    r.label <- Display_text.clean label;
    touch r.disp
  end

let set_metric r metric =
  r.metric <- metric;
  r.disp.count_cell <- max r.disp.count_cell (count_width metric);
  touch r.disp

let set_count r ~cur ~total =
  check_open "set_count" r.disp;
  if cur < 0 || total < 0 then
    invalid_arg "Console.Display.set_count: negative progress";
  if is_running r then
    match r.metric with
    | Bytes _ -> invalid_arg "Console.Display.set_count: the task counts bytes"
    | Spin -> set_metric r (Count (cur, max cur total))
    | Count (c, n) ->
        let cur = max c cur in
        set_metric r (Count (cur, max cur (max n total)))

let set_bytes r ~cur ~total =
  check_open "set_bytes" r.disp;
  if cur < 0L || total < 0L then
    invalid_arg "Console.Display.set_bytes: negative progress";
  if is_running r then begin
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

(* A line is on screen once: while the row's tail holds it, nothing else
   does; it is printed above the region when it leaves the tail. *)
let log ?(level = `Info) ?(keep = true) r text =
  let t = r.disp in
  check_open "log" t;
  let tailed = is_running r && is_live t && r.log_lines > 0 in
  List.iter
    (fun message ->
      let now = t.now () in
      let log = { gap = now -. r.log_prev; level; message; kept = keep } in
      r.log_prev <- now;
      if tailed then begin
        Queue.add log r.logs;
        if Queue.length r.logs > r.log_lines then
          let old = Queue.take r.logs in
          if old.kept then print t [ Display_row.committed_log r old ]
      end
      else if keep then
        print t
          [
            (if is_live t then Display_row.committed_log r log
             else Display_row.appended_log r log);
          ])
    (Display_text.lines text);
  touch t

let succeed ?message r =
  check_open "succeed" r.disp;
  finish `Succeeded ?message r

let fail ?message r =
  check_open "fail" r.disp;
  finish `Failed ?message r

let cancel ?message r =
  check_open "cancel" r.disp;
  finish `Cancelled ?message r

let finish_running outcome ?message t =
  List.iter (fun r -> finish outcome ?message r) (List.rev (running_rows t))

let set_result ?detail t result =
  check_open "set_result" t;
  if Option.is_none t.result then begin
    (match result with
    | `Ok _ -> finish_running `Succeeded t
    | `Fail _ -> finish_running `Cancelled t);
    let result =
      match result with
      | `Ok m -> `Ok (Display_text.clean m)
      | `Fail m -> `Fail (Display_text.clean m)
    in
    let detail =
      match detail with None -> [] | Some d -> Display_text.lines d
    in
    t.result <- Some (result, detail);
    touch t
  end

(* {1 Closing} *)

let close ?(running = `Cancel) t =
  if not t.closed then begin
    finish_running
      (match running with `Succeed -> `Succeeded | `Cancel -> `Cancelled)
      t;
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
