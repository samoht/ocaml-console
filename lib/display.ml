(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let pp_bytes ppf n = Fmt.string ppf (Display_text.bytes n)
let pp_span ppf s = Fmt.string ppf (Display_text.span s)
let sanitize = Display_text.sanitize

module Line = Display_line

(** {1 Contexts} *)

type mode = [ `Auto | `Activity | `History ]
type ctx = Display_ctx.t

let ctx = Display_ctx.v

module Event = Display_event
module Activity = Display_activity

(** {1 Sessions} *)

type nonrec t = t
type task = row

(* Nested library calls compose into the domain's current display. The outer
   scope alone owns configuration, completion and terminal teardown. A DLS
   binding keeps independent domains separate; the activity claim still
   prevents two domains from repainting the same terminal concurrently. *)
let current = Domain.DLS.new_key (fun () -> None)
let pp = Display_state.pp

let validate_geometry ~width ~height =
  (match width with
  | Some width when width <= 0 ->
      invalid_arg "Console.Display.run: width must be positive"
  | _ -> ());
  match height with
  | Some height when height <= 0 ->
      invalid_arg "Console.Display.run: height must be positive"
  | _ -> ()

let state ~(ctx : Display_ctx.t) ~plain ~styling ~theme ~palette ~bar ~width
    ~height ~header =
  let auto_width = width = None and auto_height = height = None in
  let term_w, term_h = ctx.dimensions () in
  let width = match width with Some w -> w | None -> max 1 term_w in
  let height = match height with Some h -> h | None -> max 1 term_h in
  {
    dppf = ctx.ppf;
    plain;
    styling;
    depth = ctx.depth;
    now = ctx.now;
    wait = ctx.wait;
    dimensions = ctx.dimensions;
    header = Option.map sanitize header;
    default_style = bar;
    theme;
    palette = Option.value palette ~default:(Theme.palette theme);
    dstart = ctx.now ();
    width;
    height;
    auto_width = auto_width && not plain;
    auto_height = auto_height && not plain;
    rows = [];
    count_cell = 0;
    by_key = Hashtbl.create 16;
    events_rev = [];
    next_task_id = 0;
    pending = None;
    finished = 0;
    live_height = 0;
    last_lines = [];
    painted_at = neg_infinity;
    torn_down = false;
    owns_activity = false;
    suspended = 0;
    frame = 0;
    drawn = -1;
    frame_waiters = [];
    scene = None;
  }

let release_activity t =
  if t.owns_activity then begin
    t.owns_activity <- false;
    Display_term.set_active None;
    Display_term.release ()
  end

let take_activity t =
  if not (Display_term.claim ()) then
    invalid_arg "Console.Display.run: another activity display is active"
  else begin
    t.owns_activity <- true;
    match Display_projection.install t with
    | () -> t
    | exception exn ->
        release_activity t;
        Display_term.show_cursor_on t.dppf;
        raise exn
  end

let v ~ctx ?(mode = `Auto) ?(theme = Theme.unicode) ?palette
    ?(bar = Line.default) ?width ?height ?header () =
  validate_geometry ~width ~height;
  let plain =
    match mode with
    | `History -> true
    | `Activity -> false
    | `Auto -> not ctx.Display_ctx.is_tty
  in
  (match Display_term.active () with
  | Some region when plain && region.ppf == ctx.ppf ->
      invalid_arg "Console.Display.run: output formatter is already active"
  | Some _ | None -> ());
  (* Whether the surface takes ANSI is the formatter's own answer, read once
     here: a terminal whose reader turned colour off still gets the region. *)
  let styling = Ansi.should_style ctx.ppf in
  let t =
    state ~ctx ~plain ~styling ~theme ~palette ~bar ~width ~height ~header
  in
  if plain then t else take_activity t

let touch r = Display_projection.paint_if_stale r.disp

(* A row's accent: an explicit [color], else the next palette colour (fixed at
   creation, so it is stable across redraws), else the base style's own. *)
let row_style t ~bar ~color =
  let base = Option.value bar ~default:t.default_style in
  match color with
  | Some c -> Line.with_color c base
  | None -> (
      match (bar, t.palette) with
      | None, (_ :: _ as pal) ->
          Line.with_color
            (List.nth pal (List.length t.rows mod List.length pal))
            base
      | _ -> base)

let check_open fn t =
  if t.torn_down then invalid_arg ("Console.Display." ^ fn ^ ": closed display")

let with_output t f =
  check_open "with_output" t;
  let call () =
    let margin = Format.pp_get_margin t.dppf () in
    Format.pp_set_margin t.dppf t.width;
    Fun.protect
      (fun () ->
        match f t.dppf with
        | value ->
            Format.pp_print_flush t.dppf ();
            value
        | exception exn ->
            Format.pp_print_flush t.dppf ();
            raise exn)
      ~finally:(fun () -> Format.pp_set_margin t.dppf margin)
  in
  if t.plain || t.suspended > 0 then call ()
  else
    match Display_term.active () with
    | Some region when region.ppf == t.dppf -> (
        region.clear ();
        t.suspended <- t.suspended + 1;
        let resume ~preserve =
          t.suspended <- t.suspended - 1;
          if preserve then
            try Display_projection.paint t with Sys_error _ -> ()
          else Display_projection.paint t
        in
        match call () with
        | value ->
            resume ~preserve:false;
            value
        | exception exn ->
            resume ~preserve:true;
            raise exn)
    | Some _ | None ->
        invalid_arg "Console.Display.with_output: display does not own output"

(** {1 History} *)

let record ?task ?detail t kind message =
  let event : Event.t =
    {
      elapsed = t.now () -. t.dstart;
      task = Option.map (fun r -> r.rid) task;
      key = Option.bind task (fun r -> r.rkey);
      path = Option.fold ~none:[] ~some:task_path task;
      kind;
      message;
      detail;
    }
  in
  t.events_rev <- event :: t.events_rev

let history t = List.rev t.events_rev
let activity = Display_activity.of_state

let emit_history_line t ~plain ~styled =
  if t.plain then Fmt.pf t.dppf "%s@." plain
  else Display_projection.commit t styled

(** {1 Tasks} *)

let validate_task ~id ~parent ~log_lines ~total t =
  check_open "task" t;
  if log_lines < 0 then invalid_arg "Console.Display.task: negative log_lines";
  (match total with
  | Some total when total < 0 ->
      invalid_arg "Console.Display.task: negative total"
  | Some _ | None -> ());
  (match id with
  | Some "" -> invalid_arg "Console.Display.task: empty id"
  | Some _ | None -> ());
  match parent with
  | Some parent when parent.disp != t ->
      invalid_arg "Console.Display.task: parent belongs to another display"
  | Some _ | None -> ()

(* The first sighting of an id fixes its parent and every later one keeps it.
   An event stream carries stable ids across whatever scopes the emitter is
   in, so a later parent describes where the work is now, and is ignored
   rather than fatal: a drawing surface does not get to abort its host. *)
let existing_task ~id t = Option.bind id (Hashtbl.find_opt t.by_key)

let add_task ~id ~parent ~bar ~color ~log_lines ~total ~transient ~group t label
    =
  let now = t.now () in
  let rid = t.next_task_id in
  t.next_task_id <- rid + 1;
  let inherited =
    match Option.bind parent (fun parent -> parent.routcome) with
    | Some `Succeeded -> Some `Succeeded
    | Some (`Failed | `Cancelled) -> Some `Cancelled
    | None -> None
  in
  let r =
    {
      rid;
      rkey = id;
      disp = t;
      parent;
      style = row_style t ~bar ~color;
      log_lines;
      logs = [];
      log_prev_t = now;
      rlabel = sanitize label;
      metric =
        (match total with Some total -> Count (0, total) | None -> Spin);
      transient;
      group;
      requested = None;
      swept = false;
      waiting = false;
      spilled = [];
      rstart = now;
      rfinish = Option.map (Fun.const now) inherited;
      rmessage = None;
      routcome = inherited;
      last_cur = 0L;
      last_t = now;
      rate = 0.;
    }
  in
  t.rows <- t.rows @ [ r ];
  Option.iter (fun id -> Hashtbl.add t.by_key id r) id;
  record ~task:r t `Started r.rlabel;
  Option.iter
    (fun outcome ->
      t.finished <- t.finished + 1;
      record ~task:r t (`Finished outcome) r.rlabel)
    inherited;
  touch r;
  r

let task ?id ?parent ?bar ?color ?(log_lines = 8) ?total ?(transient = false)
    ?(group = false) t label =
  validate_task ~id ~parent ~log_lines ~total t;
  match existing_task ~id t with
  | Some existing -> existing
  | None ->
      add_task ~id ~parent ~bar ~color ~log_lines ~total ~transient ~group t
        label

let set_label r label =
  check_open "set_label" r.disp;
  if r.rfinish = None then begin
    r.rlabel <- sanitize label;
    touch r
  end

(* One log line, written where history goes: above the activity region on a
   terminal, straight to the output without one, or held with its row while
   the row waits for its group. *)
let commit_log t r entry =
  if waits_for_group r then r.spilled <- r.spilled @ [ entry ]
  else
    emit_history_line t
      ~plain:(Display_row.appended_log t r entry)
      ~styled:(Display_row.committed_log t r entry)

(* A line is on screen once. While the row's tail holds a line, history does
   not; history takes it when it ages out of the tail or the row finishes. A
   line with no tail to hold it (an appending display, a row that keeps none,
   a finished row) is committed where it is made. *)
let log ?(level = `Info) ?(keep = true) r line =
  check_open "log" r.disp;
  let t = r.disp in
  let lines = List.map sanitize (Display_text.lines line) in
  let gap_of l =
    let now = t.now () in
    let gap = now -. r.log_prev_t in
    r.log_prev_t <- now;
    (now -. t.dstart, gap, l)
  in
  List.iter
    (fun line ->
      let time, gap, line = gap_of line in
      if keep then record ~task:r t (`Log level) line;
      let entry =
        {
          ltime = time;
          lgap = gap;
          llevel = level;
          lmessage = line;
          lkept = keep;
        }
      in
      let tailed = r.rfinish = None && r.log_lines > 0 in
      let held = tailed && not t.plain in
      if (not held) && keep then commit_log t r entry;
      if tailed then begin
        r.logs <- r.logs @ [ entry ];
        if List.length r.logs > r.log_lines then
          match r.logs with
          | oldest :: rest ->
              if held && oldest.lkept then commit_log t r oldest;
              r.logs <- rest
          | [] -> ()
      end)
    lines;
  touch r

let event ?(level = `Info) t message =
  check_open "event" t;
  let lines =
    match Display_text.lines message with [] -> [ "" ] | lines -> lines
  in
  List.iter
    (fun line ->
      let message = sanitize line in
      record t (`Message level) message;
      let line = Display_row.event t level message in
      emit_history_line t ~plain:line ~styled:line)
    lines

(** {1 Progress} *)

(* Every metric change goes through here, so the block's count cell widens with
   the row that needs it. *)
let set_metric r metric =
  r.metric <- metric;
  r.disp.count_cell <- max r.disp.count_cell (count_width metric);
  touch r

let set_count r ~cur ~total =
  check_open "set_count" r.disp;
  if cur < 0 || total < 0 then
    invalid_arg "Console.Display.set_count: expected non-negative progress";
  if r.rfinish = None then begin
    let total = max cur total in
    let cur, total =
      match r.metric with
      | Spin -> (cur, total)
      | Count (previous, previous_total) ->
          let cur = max previous cur in
          (cur, max cur (max previous_total total))
      | Bytes _ ->
          invalid_arg "Console.Display.set_count: task already tracks bytes"
    in
    set_metric r (Count (cur, total))
  end

let advance ?(by = 1) ?total ?label r =
  check_open "advance" r.disp;
  if by < 0 then invalid_arg "Console.Display.advance: negative increment";
  (match total with
  | Some total when total < 0 ->
      invalid_arg "Console.Display.advance: negative total"
  | Some _ | None -> ());
  if r.rfinish = None then begin
    Option.iter (fun label -> r.rlabel <- sanitize label) label;
    match r.metric with
    | Spin ->
        let total = max by (Option.value total ~default:by) in
        set_count r ~cur:by ~total
    | Count (cur, known_total) ->
        let cur = if by > max_int - cur then max_int else cur + by in
        let total = max cur (max known_total (Option.value total ~default:0)) in
        set_count r ~cur ~total
    | Bytes _ ->
        invalid_arg "Console.Display.advance: task already tracks bytes"
  end

let set_bytes r ~cur ~total =
  check_open "set_bytes" r.disp;
  if cur < 0L || total < 0L then
    invalid_arg "Console.Display.set_bytes: expected non-negative progress";
  if r.rfinish = None then begin
    let total = Int64.max cur total in
    let cur, total =
      match r.metric with
      | Spin -> (cur, total)
      | Bytes (previous, previous_total) ->
          let cur = Int64.max previous cur in
          (cur, Int64.max cur (Int64.max previous_total total))
      | Count _ ->
          invalid_arg "Console.Display.set_bytes: task already tracks count"
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

(** {1 Endings} *)

(* The lines that waited for [r], depth first in the order the rows started:
   each row's kept log lines, then the row's own line, then the rows under it.
   A row that waited for an inner group is still waiting when that group
   settles under an outer one, and is written here with the rest. *)
let rec waited_lines t r =
  List.concat_map
    (fun child ->
      let logs = List.map (Display_row.committed_log t child) child.spilled in
      child.spilled <- [];
      let own =
        if child.waiting then begin
          child.waiting <- false;
          [ Display_row.committed t child ]
        end
        else []
      in
      logs @ own @ waited_lines t child)
    (children r)

(* A group asked to end while rows under it are open keeps the first ending it
   was asked for and settles with it when its last open row settles, so its
   cost runs to that moment. Any other row settles at once, its open
   descendants first.

   A [swept] row never reported an ending of its own: the row it hung under
   ended while it was still running. It commits no permanent line, since the
   only sentence the display could write for it would be about its own shape;
   the event stream still carries its ending. *)
let rec finish ?(swept = false) outcome ?message r =
  check_open "finish" r.disp;
  if r.rfinish = None then
    if (not swept) && r.group && running_children r <> [] then
      r.requested <- Some (Option.value r.requested ~default:(outcome, message))
    else settle ~swept outcome ?message r

and settle ~swept outcome ?message r =
  let child_outcome =
    match outcome with
    | `Succeeded -> `Succeeded
    | `Failed | `Cancelled -> `Cancelled
  in
  List.iter
    (fun child -> finish ~swept:true child_outcome child)
    (running_children r);
  let t = r.disp in
  (* The row is about to leave the live region, so history takes its tail
     before the line that closes the row. *)
  if not t.plain then begin
    List.iter (fun log -> if log.lkept then commit_log t r log) r.logs;
    r.logs <- []
  end;
  r.rfinish <- Some (t.now ());
  r.rmessage <- Option.map sanitize message;
  r.routcome <- Some outcome;
  r.swept <- swept;
  t.finished <- t.finished + 1;
  let message = Option.value r.rmessage ~default:r.rlabel in
  record ~task:r t (`Finished outcome) message;
  let own = not (swept || r.transient) in
  (if waits_for_group r then r.waiting <- own
   else if t.plain then (
     if own then Fmt.pf t.dppf "%s@." (Display_row.appended t r))
   else
     match
       (if own then [ Display_row.committed t r ] else []) @ waited_lines t r
     with
     | [] -> ()
     | lines -> Display_projection.commit t (String.concat "\r\n" lines));
  match r.parent with
  | Some parent when not swept -> settle_requested parent
  | Some _ | None -> ()

and settle_requested group =
  match group.requested with
  | Some (outcome, message)
    when group.rfinish = None && running_children group = [] ->
      settle ~swept:false outcome ?message group
  | Some _ | None -> ()

let succeed ?message r = finish `Succeeded ?message r
let fail ?message r = finish `Failed ?message r
let cancel ?message r = finish `Cancelled ?message r

let with_task ?id ?parent ?bar ?color ?log_lines ?total ?transient ?group t
    label f =
  let task =
    task ?id ?parent ?bar ?color ?log_lines ?total ?transient ?group t label
  in
  match f task with
  | value ->
      succeed task;
      value
  | exception exn ->
      fail task;
      raise exn

let finish_running outcome ?message t =
  List.iter
    (fun task -> if task.rfinish = None then finish outcome ?message task)
    (List.rev t.rows)

let set_result ?detail t result =
  check_open "set_result" t;
  if Option.is_none t.pending then begin
    (match result with
    | `Ok _ -> finish_running `Succeeded t
    | `Fail _ -> finish_running `Cancelled ~message:"session failed" t);
    let result =
      match result with
      | `Ok message -> `Ok (sanitize message)
      | `Fail message -> `Fail (sanitize message)
    in
    let detail = Option.map Display_text.sanitize_multiline detail in
    t.pending <- Some (result, detail);
    let kind, message =
      match result with
      | `Ok message -> (`Result `Ok, message)
      | `Fail message -> (`Result `Fail, message)
    in
    record ?detail t kind message
  end

(** {1 Frames and the clock} *)

let tick t =
  if not t.plain then begin
    t.frame <- t.frame + 1;
    Display_projection.paint t
  end

let frame t = t.frame

let on_frame t n k =
  if t.plain || t.torn_down || t.drawn >= n then k ()
  else t.frame_waiters <- (n, k) :: t.frame_waiters

let set_scene t scene =
  check_open "set_scene" t;
  t.scene <- Some scene;
  Display_projection.paint_if_stale t

let now t = t.now ()
let wait t = t.wait

(** {1 Teardown} *)

let close t =
  if not t.torn_down then begin
    let result, detail =
      match t.pending with
      | Some (result, detail) -> (Some result, detail)
      | None -> (None, None)
    in
    t.torn_down <- true;
    let detail = Display_summary.detail t detail in
    let banner = Option.map (Display_summary.verdict t) result in
    if t.plain then begin
      (match Display_summary.header t ~fit:false with
      | Some s -> Fmt.pf t.dppf "%s@." s
      | None -> ());
      List.iter (fun s -> Fmt.pf t.dppf "%s@." s) detail;
      (match banner with Some s -> Fmt.pf t.dppf "%s@." s | None -> ());
      Format.pp_print_flush t.dppf ()
    end
    else
      let line s =
        Format.pp_print_string t.dppf s;
        Format.pp_print_string t.dppf "\r\n"
      in
      (* The cursor is shown before the closing lines, so the last byte the
         display writes ends a line and what the program writes next starts
         one. *)
      let shown = ref false in
      Fun.protect
        (fun () ->
          Display_term.with_unlimited_margin t.dppf (fun () ->
              Display_term.pp_clear t.dppf t.live_height;
              Format.pp_print_string t.dppf Display_term.show_cursor;
              shown := true;
              (match Display_summary.header t ~fit:true with
              | Some s -> line s
              | None -> ());
              List.iter line detail;
              match banner with Some s -> line s | None -> ()))
        ~finally:(fun () ->
          t.live_height <- 0;
          if !shown then
            try Format.pp_print_flush t.dppf () with Sys_error _ -> ()
          else Display_term.show_cursor_on t.dppf;
          release_activity t)
  end

let cancel_running t message =
  List.iter
    (fun task -> if task.rfinish = None then cancel ~message task)
    (List.rev t.rows)

(* Runs while another exception unwinds: a write failure on the display's
   channel is dropped so that the original exception is the one that
   propagates. *)
let cleanup_after_exception t =
  try
    cancel_running t "interrupted";
    close t
  with Sys_error _ ->
    t.torn_down <- true;
    if not t.plain then begin
      Display_term.show_cursor_on t.dppf;
      release_activity t
    end

let run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Domain.DLS.get current with
  | Some t -> f t
  | None ->
      let t = v ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header () in
      Domain.DLS.set current (Some t);
      Fun.protect
        ~finally:(fun () ->
          Domain.DLS.set current None;
          Display_projection.release_frames t)
        (fun () ->
          match f t with
          | value ->
              finish_running `Succeeded t;
              close t;
              value
          | exception exn ->
              cleanup_after_exception t;
              raise exn)
