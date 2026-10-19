(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_state

let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* A redraw clears the old region with cursor-relative moves, which cannot
   reach a line that scrolled into scrollback, so the region stays within the
   terminal height. The rows always stay; the log tails are shed, oldest first,
   with a marker counting what was hidden. If even the bare rows overflow, the
   first and the most recent ones stay. *)
let region_marker t hidden =
  Width.truncate t.width (Fmt.kstr (dimmed t) "  ... (%d more lines)" hidden)

let cap_rows t ~header ~budget ~total blocks =
  let rows = List.map fst blocks in
  match rows with
  | [] -> header
  | first :: rest ->
      if budget = 1 then header @ [ List.hd (List.rev rows) ]
      else if budget = 2 then header @ [ first; List.hd (List.rev rest) ]
      else
        let shown = drop (List.length rest - (budget - 2)) rest in
        let hidden = total - 1 - List.length shown in
        header @ (first :: region_marker t hidden :: shown)

let cap_logs t ~header ~budget ~total blocks =
  if List.length blocks = budget then header @ List.map fst blocks
  else
    let to_drop = total - (budget - 1) in
    let flat =
      List.concat_map
        (fun (line, logs) -> `Row line :: List.map (fun l -> `Log l) logs)
        blocks
    in
    let rec shed dropped acc = function
      | rest when dropped >= to_drop ->
          List.rev_append acc (List.map (function `Row s | `Log s -> s) rest)
      | `Log _ :: tl -> shed (dropped + 1) acc tl
      | `Row s :: tl -> shed dropped (s :: acc) tl
      | [] -> List.rev acc
    in
    header @ (region_marker t to_drop :: shed 0 [] flat)

(* One row stays free for the cursor: emitting beyond it would scroll an old
   frame out of reach of the relative clear. *)
let capacity t = max 0 (t.height - 1)

let cap_region t ~header ~blocks =
  let capacity = capacity t in
  let header = if capacity = 0 then [] else header in
  let budget = max 0 (capacity - List.length header) in
  let total =
    List.fold_left (fun n (_, logs) -> n + 1 + List.length logs) 0 blocks
  in
  if budget = 0 then header
  else if total <= budget then
    header @ List.concat_map (fun (line, logs) -> line :: logs) blocks
  else if List.length blocks > budget then
    cap_rows t ~header ~budget ~total blocks
  else cap_logs t ~header ~budget ~total blocks

let rec row_live row =
  row.rfinish = None
  && match row.parent with None -> true | Some parent -> row_live parent

let lines t =
  let header =
    match Display_summary.header t ~fit:true with Some s -> [ s ] | None -> []
  in
  let active = List.filter row_live t.rows in
  let blocks =
    List.map
      (fun row -> (Display_row.live t row, Display_row.tail t row))
      active
  in
  cap_region t ~header ~blocks

(* A resize between frames is picked up before the next clear and redraw: a
   stale width lets a line wrap and a stale height mis-sizes the cap. *)
let refresh_size t =
  let old_width = t.width and old_height = t.height in
  if t.auto_width || t.auto_height then begin
    let width, height = t.dimensions () in
    if t.auto_width then t.width <- max 1 width;
    if t.auto_height then t.height <- max 1 height
  end;
  t.width <> old_width || t.height <> old_height

let paint t =
  if (not t.plain) && (not t.torn_down) && t.suspended = 0 then begin
    t.painted_at <- t.now ();
    let resized = refresh_size t in
    let lines = lines t in
    (* A frame byte-identical to the screen is not rewritten. [live_height] is
       0 after a clear, so a cleared region never matches. *)
    if lines <> t.last_lines || List.length lines <> t.live_height then begin
      Display_term.with_unlimited_margin t.dppf (fun () ->
          if
            (not resized) && t.live_height > 0
            && List.length lines = t.live_height
          then
            Display_term.pp_update t.dppf ~old_lines:t.last_lines
              ~new_lines:lines
          else begin
            Display_term.pp_clear t.dppf t.live_height;
            Display_term.pp_block t.dppf lines
          end;
          Format.pp_print_flush t.dppf ());
      (* No trailing newline: on the bottom screen row it would scroll the
         region up every paint and strand the header in scrollback. *)
      t.live_height <- List.length lines;
      t.last_lines <- lines
    end
  end

let clear t =
  Display_term.with_unlimited_margin t.dppf (fun () ->
      Display_term.pp_clear t.dppf t.live_height;
      Format.pp_print_flush t.dppf ());
  t.live_height <- 0

(* History is not truncated: it may wrap, but the region was cleared first and
   [live_height] reset, so a wrapped line cannot corrupt the next paint. *)
let commit t s =
  ignore (refresh_size t);
  Display_term.with_unlimited_margin t.dppf (fun () ->
      Display_term.pp_clear t.dppf t.live_height;
      Format.pp_print_string t.dppf s;
      Format.pp_print_string t.dppf "\r\n";
      Format.pp_print_flush t.dppf ());
  t.live_height <- 0;
  paint t

let install t =
  Display_term.restore_at_exit ();
  Format.pp_print_string t.dppf Display_term.hide_cursor;
  Format.pp_print_flush t.dppf ();
  paint t

let redraw_interval = 0.1

let paint_if_stale t =
  if (not t.plain) && t.now () -. t.painted_at >= redraw_interval then paint t
